default:
    cargo fmt
    cargo build
    cd ./auto-test && python3.10 test_compiler ../target/debug/compiler --chapter 4

run filename:
    cargo fmt
    cargo run {{filename}}