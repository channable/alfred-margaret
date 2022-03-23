extern crate aho_corasick;

use aho_corasick::{AcAutomaton, Automaton, Dense, Sparse};

#[repr(C)]
#[derive(Debug)]
pub struct U8Slice {
    ptr: *const u8,
    off: isize,
    len: isize,
}

impl U8Slice {
    fn into_slice<'a>(&self) -> &'a [u8] {
        slice_from_pointer(self.ptr, self.off, self.len)
    }
}

// https://doc.rust-lang.org/src/core/slice/raw.rs.html#87
fn slice_from_pointer<'a, T>(ptr: *const T, off: isize, len: isize) -> &'a [T] {
    &(unsafe { &*std::ptr::slice_from_raw_parts(ptr, (off + len) as usize) })[off as usize..]
}

#[no_mangle]
pub extern "C" fn perform_ac(
    use_sparse: bool,
    num_needles: isize,
    needle_slices_: *const U8Slice,
    haystack_slice_: *const U8Slice,
) -> isize {
    let needle_slices = slice_from_pointer(needle_slices_, 0, num_needles);
    // TODO: Can we somehow allocate this on the stack?
    let mut needles: Vec<&[u8]> = Vec::with_capacity(num_needles as usize);
    for i in 0..num_needles as usize {
        needles.insert(i, needle_slices[i].into_slice());
    }

    let haystack = slice_from_pointer(haystack_slice_, 0, 1)[0].into_slice();

    let num_matches = if use_sparse {
        let automaton = AcAutomaton::<_, Sparse>::with_transitions(&needles[..]);
        automaton.find_overlapping(haystack).count()
    } else {
        let automaton = AcAutomaton::<_, Dense>::with_transitions(&needles[..]);
        automaton.find_overlapping(haystack).count()
    };

    num_matches as isize
}
