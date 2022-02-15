import java.io.IOException;
import java.lang.StringBuilder;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import com.hankcs.algorithm.AhoCorasickDoubleArrayTrie;
import com.hankcs.algorithm.AhoCorasickDoubleArrayTrie.IHit;

class MatchCounter implements IHit<String> {
  public int count;

  public MatchCounter() {
    this.count = 0;
  }

  public void hit(int begin, int end, String value) {
    this.count++;
  }
}

class AcBench {
  public static void main(String[] args) {
    for (int i = 0; i < args.length; i++) {
      try {
        AcBench.processFile(args[i]);
      } catch (IOException ex) {
        System.out.println(ex.toString());
        System.exit(1);
      }
    }
  }

  static void processFile(String filename) throws IOException {
    // Preparation step: read the file, needles first, then the haystack.
    Charset utf16le = Charset.forName("UTF-16LE");
    List<String> fileLines = Files.readAllLines(Paths.get(filename), utf16le);
    TreeMap<String, String> needles = new TreeMap<String, String>();
    StringBuilder haystackBuilder = new StringBuilder();

    boolean isNeedles = true;
    for (int i = 0; i < fileLines.size(); i++) {
      String line = fileLines.get(i);

      if (line.equals("")) {
        // A blank line signals the end of the needle section.
        isNeedles = false;
        continue;
      }

      if (isNeedles) {
        needles.put(line, line);
      } else {
        haystackBuilder.append(line);
        haystackBuilder.append('\n');
      }
    }

    String haystack = haystackBuilder.toString();

    // Now for the real work: build the automaton and print the number of matches.

    // Measure every input 5 times.
    for (int i = 0; i < 5; i++) {
      MatchCounter matchCounter = new MatchCounter();

      long epoch = System.nanoTime();
      if (needles.size() > 0) {
        AhoCorasickDoubleArrayTrie<String> automaton = new AhoCorasickDoubleArrayTrie<String>();
        automaton.build(needles);
        automaton.parseText(haystack, matchCounter);
      }
      long durationNs = System.nanoTime() - epoch;

      if (i == 0) {
        // Only print count to stderr the first iteration, to check correctness.
        System.err.format("%d\n", matchCounter.count);
      }

      System.out.format("%d\t", durationNs);
    }

    // Print a newline, so we print one line of measurements per file.
    System.out.println("");
  }
}
