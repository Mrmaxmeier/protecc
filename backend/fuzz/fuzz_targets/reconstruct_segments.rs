#![no_main]
use libfuzzer_sys::fuzz_target;

use snacc::reassembly::StreamReassembly;
use snacc::stream::Stream;

fuzz_target!(|stream_reassembly: StreamReassembly| {
    Stream::reconstruct_segments(stream_reassembly);
});
