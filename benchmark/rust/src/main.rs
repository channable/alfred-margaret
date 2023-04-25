extern crate aho_corasick;
extern crate filebuffer;
extern crate memchr;

use std::env;
use std::io;

use aho_corasick::{AcAutomaton, Automaton, Dense};
use filebuffer::FileBuffer;
use memchr::memchr;

/// Print the number of matches found in the haystack in the file, and timings.
///
/// The file format consists of needles, one per line, followed by a blank line,
/// followed by the haystack. The file is encoded as UTF-16 LE without byte
/// order mark.
fn process_file(file_name: String) -> io::Result<()> {
    let file = FileBuffer::open(file_name)?;

    let mut needles = Vec::with_capacity(1000);
    let mut start = 0;

    loop {
        // The UTF-16 LE newline sequence is an ascii newline (0x0a) and a null
        // byte. Locate the newline byte (0x0a) first, then check that it is
        // part of 0x0a, 0x00 with the proper alignment.
        let mut len = 0;
        loop {
            len += memchr(0x0a, &file[start + len..]).expect("Unexpected EOF before haystack");
            if len % 2 != 0 {
                // We are looking for the first byte of the 16-bit integer,
                // not the second one.
                continue;
            }

            if let Some(0x00) = file.get(start + len + 1) {
                // The second byte is as expected, this really was a newline.
                break;
            }

            // Skip over the 0x0a that is not a newline on the next memchr.
            len += 1;
        }

        // A blank line ends the needle section of the file, what follows is
        // haystack.
        if len == 0 {
            break;
        }

        // The newline byte is not part of the needle.
        let end = start + len;
        needles.push(&file[start..end]);

        // The next needle starts after the null byte after the newline byte.
        start = start + len + 2;
    }

    let haystack = &file[start..];

    // Run the benchmark 5 times.
    for i in 0..5 {
        let epoch = std::time::Instant::now();
        // NOTE: You can opt for aho_corasick::Dense or Sparse. Dense is more memory
        // efficient and Sparse is faster according to the docs, however, in my
        // measurements, Dense is faster. You can also leave off the .into_full()
        // which is also more memory efficient but slower according to the docs,
        // and also according to my measurements.
        // TODO: Re-enable .into_full() once https://github.com/BurntSushi/aho-corasick/issues/35
        // is fixed.
        let automaton = AcAutomaton::<_, Dense>::with_transitions(&needles[..]);
        let num_matches = automaton.find_overlapping(haystack).count();

        let duration = epoch.elapsed();

        // Print duration in nanoseconds, tab separated.
        print!("{}\t", duration.as_nanos());

        // In the first iteration, print the match count to stderr, so we can
        // verify it against the reference implementation for correctness.
        if i == 0 {
            eprintln!("{}", num_matches);
        }
    }

    // Print a newline, so we print one line of measurements per file.
    println!("");

    Ok(())
}

fn main() {
    // Skip the program name.
    for file_name in env::args().skip(1) {
        process_file(file_name).unwrap();
    }
}
