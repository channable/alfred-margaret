extern crate aho_corasick;

use aho_corasick::Automaton;
use aho_corasick::{AcAutomaton, Dense};

// https://doc.rust-lang.org/src/core/slice/raw.rs.html#87
fn slice_from_pointer<'a, T>(ptr: *const T, off: isize, len: isize) -> &'a [T] {
    &(unsafe { &*std::ptr::slice_from_raw_parts(ptr, (off + len) as usize) })[off as usize..]
}

#[no_mangle]
pub extern "C" fn perform_ac(
    num_needles: isize,
    buffers_: *const *const u8,
    offs_: *const isize,
    lens_: *const isize,
    haystack_buf: *const u8,
    haystack_off: isize,
    haystack_len: isize,
) -> isize {
    let buffers = slice_from_pointer(buffers_, 0, num_needles);
    let offs = slice_from_pointer(offs_, 0, num_needles);
    let lens = slice_from_pointer(lens_, 0, num_needles);

    let mut needles = Vec::with_capacity(num_needles as usize);
    for i in 0..num_needles as usize {
        let needle = slice_from_pointer(buffers[i], offs[i], lens[i]);
        needles.push(needle);
    }

    let haystack = slice_from_pointer(haystack_buf, haystack_off, haystack_len);

    let automaton = AcAutomaton::<_, Dense>::with_transitions(&needles[..]);
    let num_matches = automaton.find_overlapping(haystack).count();

    num_matches as isize
}
