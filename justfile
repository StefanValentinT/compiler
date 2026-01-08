default:
    cargo build
    cd ./auto-test && python3.10 test_compiler ../target/debug/compiler --chapter 1 --stage lex

run filename:
    cargo run {{filename}}