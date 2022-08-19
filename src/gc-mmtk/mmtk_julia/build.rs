// Copyright 2017 The Australian National University
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

extern crate built;

extern crate cc;

#[cfg(any(target_os = "macos", target_os = "linux"))]
#[cfg(target_arch = "x86_64")]
fn main() {
    cc::Build::new()
        .flag("-O3")
        .flag("-fPIC")
        .flag("-c")
        .file("runtime/runtime_gc_x64.c")
        .compile("libruntime_gc_c.a");

    built();
}

fn built() {
    built::write_built_file()
        .expect("Failed to acquire build-time information");
}
