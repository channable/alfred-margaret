{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Criterion.Main (bgroup, bench, defaultMain, nf)

import qualified Data.Text as Text
import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.BoyerMoore.Automaton as BoyerMoore
import qualified Data.Text.BoyerMooreCI.Automaton as BoyerMooreCI
import qualified Data.Text.Utf8 as Utf8

--
-- @
-- stack bench alfred-margaret:bm --ba '--output bm.html'
-- @

main :: IO ()
main = defaultMain
  [
    bgroup "Big needle, large haystack" $
      benchVariants needle1 haystack1

  , bgroup "Big needle, small haystack" $
      benchVariants needle2 haystack2

  , bgroup "Small needle, large haystack" $
      benchVariants needle3 haystack3

  , bgroup "Small needle, small haystack" $
      benchVariants needle4 haystack4

  , bgroup "Cyrillic, medium needle, large haystack" $
      benchVariants needle5 haystack5

  ]
  where
    benchVariants needle haystack =
      [ bench "Count Aho" $ nf (ahoCount needle) haystack
      , bench "Count Aho CI" $ nf (ahociCount needle) haystack
      , bench "Count BoyerMoore" $ nf (bmCount needle) haystack
      , bench "Count BoyerMoore CI" $ nf (bmciCount needle) haystack
      , bench "Count Aho CI multineedle" $ nf (ahocimultiCount needle) haystack
      , bench "Count Text.count" $ nf (Text.count needle) haystack
      -- This naive case insensitive count is really slow. We leave it out
      -- because it throws off the plot scale.
      -- , bench "Count Text.count . lowerUtf8" $ nf (Text.count needle . Utf8.lowerUtf8) haystack
      , bench "Contains Aho" $ nf (ahoContains needle) haystack
      , bench "Contains Aho CI" $ nf (ahociContains needle) haystack
      , bench "Contains BoyerMoore" $ nf (bmContains needle) haystack
      , bench "Contains BoyerMoore CI" $ nf (bmciContains needle) haystack
      , bench "Contains Text.isInfixOf" $ nf (Text.isInfixOf needle) haystack
      ]


-- | The NOINLINE annotation is used so that the needle doesn't get inlined at compile time. The
-- function does get partially applied with the needle so that the automaton is constructed only
-- once.
ahoCount :: Text.Text -> Text.Text -> Int
{-# NOINLINE ahoCount #-}
ahoCount needle =
  let
    !automaton = Aho.build [(needle, ())]
    onMatch !n _match = Aho.Step (n + 1)
  in
    -- Eta expansion of the haystack variable is necessary for proper inlining of runText
    \haystack -> Aho.runText 0 onMatch automaton haystack

ahoContains :: Text.Text -> Text.Text -> Bool
{-# NOINLINE ahoContains #-}
ahoContains needle =
  let
    !automaton = Aho.build [(needle, ())]
    onMatch _ _match = Aho.Done True
  in
    \haystack -> Aho.runText False onMatch automaton haystack

ahociCount :: Text.Text -> Text.Text -> Int
{-# NOINLINE ahociCount #-}
ahociCount needle =
  let
    !automaton = Aho.build [(needle, ())]
    onMatch !n _match = Aho.Step (n + 1)
  in
    \haystack -> Aho.runLower 0 onMatch automaton haystack


ahocimultiCount :: Text.Text -> Text.Text -> Int
{-# NOINLINE ahocimultiCount #-}
ahocimultiCount needle =
  let
    !automaton = Aho.build $ map (\n -> (n, ())) $ Aho.needleCasings needle
    onMatch !n _match = Aho.Step (n + 1)
  in
    \haystack -> Aho.runText 0 onMatch automaton haystack

ahociContains :: Text.Text -> Text.Text -> Bool
{-# NOINLINE ahociContains #-}
ahociContains needle =
  let
    !automaton = Aho.build [(needle, ())]
    onMatch _ _match = Aho.Done True
  in
    \haystack -> Aho.runLower False onMatch automaton haystack

bmciCount :: Text.Text -> Text.Text -> Int
{-# NOINLINE bmciCount #-}
bmciCount needle =
  let
    !automaton = BoyerMooreCI.buildAutomaton needle
    onMatch !n _match = BoyerMooreCI.Step (n + 1)
  in
    \haystack -> BoyerMooreCI.runText 0 onMatch automaton haystack

bmciContains :: Text.Text -> Text.Text -> Bool
{-# NOINLINE bmciContains #-}
bmciContains needle =
  let
    !automaton = BoyerMooreCI.buildAutomaton needle
    onMatch _ _match = BoyerMooreCI.Done True
  in
    \haystack -> BoyerMooreCI.runText False onMatch automaton haystack

bmCount :: Text.Text -> Text.Text -> Int
{-# NOINLINE bmCount #-}
bmCount needle =
  let
    !automaton = BoyerMoore.buildAutomaton needle
    onMatch !n _match = BoyerMoore.Step (n + 1)
  in
    \haystack -> BoyerMoore.runText 0 onMatch automaton haystack

bmContains :: Text.Text -> Text.Text -> Bool
{-# NOINLINE bmContains #-}
bmContains needle =
  let
    !automaton = BoyerMoore.buildAutomaton needle
    onMatch _ _match = BoyerMoore.Done True
  in
    \haystack -> BoyerMoore.runText False onMatch automaton haystack


needle1 :: Text.Text
needle1 = "necessitatibus"

haystack1 :: Text.Text
haystack1 =
  Text.unlines
    [ "Lorem ipsum dolor sit amet. Et sint voluptatibus est vero maxime vel explicabo reprehenderit non molestiae quisquam sit dolores facere qui cumque quibusdam 33 impedit deserunt! Aut libero harum et quis quasi qui cupiditate autem."
    , "Qui quia totam non rerum eveniet sed tempora repellendus ab enim consequatur eum quaerat iste. Sed amet nihil sed voluptate aspernatur ut rerum facilis est officia earum aut molestiae tenetur non autem nulla. Qui deserunt necessitatibus ab accusamus doloremque non sint aspernatur."
    , "Et officia illum non quaerat obcaecati cum accusamus minus rem quae quis sed rerum omnis sed inventore quasi. Qui totam deserunt sit minima ullam sit debitis dolores. Est debitis explicabo ut temporibus corporis nam harum dolore est fuga numquam non exercitationem Quis cum amet fuga."
    , "Aut incidunt provident et sequi nulla est molestias perferendis. Hic exercitationem modi ex optio cumque nam voluptate debitis nam iste consequatur non nihil rerum ut accusantium nihil. Sed ullam maiores nobis dolorem sit galisum maiores eum reprehenderit maxime sed galisum placeat cum molestiae quia id similique velit. Aut quasi autem non illo reiciendis sit ullam tempora."
    , "Ut corporis exercitationem sed dicta autem ut voluptatem dolorem vel dolores dolores. Qui ipsam quisquam sed facere porro rem autem necessitatibus nam beatae quisquam. Quo voluptatem optio hic quod reprehenderit ut nostrum voluptatem."
    , "Et consequuntur quia vel unde laudantium non voluptatum magnam. Ut quam autem rem fugit quia ut assumenda error quam amet in omnis quia ut rerum soluta sed consequatur fuga. Ut blanditiis quia et facilis ratione aut blanditiis dolorum aut itaque excepturi eos iste incidunt qui blanditiis velit et magni autem."
    ]

needle2 :: Text.Text
needle2 = "necessitatibus"

haystack2 :: Text.Text
haystack2 =
  Text.unlines
    [ "Lorem ipsum dolor sit amet. Et sint voluptatibus est vero maxime vel explicabo reprehenderit non molestiae quisquam sit dolores facere qui cumque quibusdam 33 impedit deserunt! Aut libero harum et quis quasi qui cupiditate autem."
    ]

needle3 :: Text.Text
needle3 = "sit"

haystack3 :: Text.Text
haystack3 =
  Text.unlines
    [ "Lorem ipsum dolor sit amet. Et sint voluptatibus est vero maxime vel explicabo reprehenderit non molestiae quisquam sit dolores facere qui cumque quibusdam 33 impedit deserunt! Aut libero harum et quis quasi qui cupiditate autem."
    , "Qui quia totam non rerum eveniet sed tempora repellendus ab enim consequatur eum quaerat iste. Sed amet nihil sed voluptate aspernatur ut rerum facilis est officia earum aut molestiae tenetur non autem nulla. Qui deserunt necessitatibus ab accusamus doloremque non sint aspernatur."
    , "Et officia illum non quaerat obcaecati cum accusamus minus rem quae quis sed rerum omnis sed inventore quasi. Qui totam deserunt sit minima ullam sit debitis dolores. Est debitis explicabo ut temporibus corporis nam harum dolore est fuga numquam non exercitationem Quis cum amet fuga."
    , "Aut incidunt provident et sequi nulla est molestias perferendis. Hic exercitationem modi ex optio cumque nam voluptate debitis nam iste consequatur non nihil rerum ut accusantium nihil. Sed ullam maiores nobis dolorem sit galisum maiores eum reprehenderit maxime sed galisum placeat cum molestiae quia id similique velit. Aut quasi autem non illo reiciendis sit ullam tempora."
    , "Ut corporis exercitationem sed dicta autem ut voluptatem dolorem vel dolores dolores. Qui ipsam quisquam sed facere porro rem autem necessitatibus nam beatae quisquam. Quo voluptatem optio hic quod reprehenderit ut nostrum voluptatem."
    , "Et consequuntur quia vel unde laudantium non voluptatum magnam. Ut quam autem rem fugit quia ut assumenda error quam amet in omnis quia ut rerum soluta sed consequatur fuga. Ut blanditiis quia et facilis ratione aut blanditiis dolorum aut itaque excepturi eos iste incidunt qui blanditiis velit et magni autem."
    ]

needle4 :: Text.Text
needle4 = "sit"

haystack4 :: Text.Text
haystack4 =
  Text.unlines
    [ "Lorem ipsum dolor sit amet. Et sint voluptatibus est vero maxime vel explicabo reprehenderit non molestiae quisquam sit dolores facere qui cumque quibusdam 33 impedit deserunt! Aut libero harum et quis quasi qui cupiditate autem."
    ]



needle5 :: Text.Text
needle5 = "сусципиантур"

-- Cyrillic lipsum from https://generator.lorem-ipsum.info/_russian
haystack5 :: Text.Text
haystack5 =
  Text.unlines
    [ "Лорем ипсум долор сит амет, ин вис вирис маиорум инсоленс, алиа агам иудицабит цу нец, ут адмодум инцидеринт еффициантур цум. Вих те номинави диспутатиони, не дуо путент дицунт. Дебет репудиандае яуо ан, вих тамяуам репудиандае цонцлусионемяуе но, цу иус регионе урбанитас. Персиус дицерет форенсибус ат цум, еум яуаеяуе аппеллантур еу. Хас вивендо цонсететур те, ад хас меис пхаедрум, репудиаре демоцритум вих еу."
    , "Пер ут нулла минимум цонтентионес, сеа ат мунере алтера медиоцрем. Но вим лабитур цонсететур, нам ут ипсум фацилис феугаит. Не дицат феугиат аппареат нам, дуис адиписцинг ех еам. Ут еуисмод веритус интерпретарис усу. Сед цасе дицунт ехпетендис ех. Ад лудус урбанитас вим."
    , "Перфецто инсоленс ут меи, адмодум ментитум партиендо усу ат. Ин солеат еирмод аперири дуо, еи мазим фацилиси делицата сед. Дицо неглегентур ут хис, еи хис афферт аудире. Ат аццусата глориатур вел."
    , "Персиус омиттам при ад, цонгуе воцент репудиаре усу ет. Ат детрахит салутатус вим. Еа сеа яуис тациматес. Ид фацер дицтас еум, вери дицам еум еу, при ид тантас омиттам. Суавитате персецути еам те, ет яуот омиттантур ест."
    , "Еа тациматес евертитур дуо, меа ин вениам ассентиор. Еос ессе мунди аццусата цу. Ат партем медиоцрем иус, ан мел алии плацерат. Еам еи суммо ерант тинцидунт. Меа еа алиа сцрибентур."
    , "Еи вих малис омиттам сплендиде, ид витае ностер алияуандо еам, те новум репудиаре пер. Вис цу еррор алтерум садипсцинг, ан ерат фацете аудире ест. Меи ат виде ассуеверит. Вис либрис популо ид, ат хис симилияуе репудиандае, нибх салутатус ех сеа."
    , "Сеа пондерум адиписцинг ех, ет нонумы анциллае вих. Цу иллум ессент медиоцрем вис, еу мовет мунди еос, яуо зрил долорем партиендо но. Хис ат диам цоммодо демоцритум, вери цаусае инцидеринт усу ин. Темпорибус диссентиунт вис еа, ех цум цонсул поссим алтерум. Вим ин веро нулла, еиус реяуе ех меа."
    , "Омнес трацтатос цу еос. Пер ехерци модератиус еа. Нец оффендит сусципит ех. Пер ех постеа фацете, те еум еиус игнота легендос. Ат поссе воцибус пер, ин про лаудем сплендиде цотидиеяуе, но адхуц инермис аццоммодаре еос. Ин денияуе малуиссет еам, ин яуис доцтус нец."
    , "Идяуе елояуентиам еу яуи, витае интерессет не вис, хис диссентиет сусципиантур цу. Тибияуе елаборарет еам еа. Оратио вивендум цонсулату ид пер. Еа путант еррорибус сит, цум ад алияуид аццоммодаре. Алтерум делецтус алияуандо еу иус, ан иус поссит персиус тибияуе. Сеа не нумяуам фуиссет, ат виси аудире при. Суас граеце цопиосае дуо ан, но мунди опортере цонвенире яуи."
    , "Мел фугит путант опортере но, цопиосае оффициис еос еа. Вис инвидунт еффициенди ат, те луцилиус сцрибентур хис, вих не поссит вертерем. Дуо омнис дицат вирис еи, ет адхуц цетеро вис. Еа вим утинам инвенире, вих ин еяуидем ассентиор."
    ]
