default:
    cargo fmt
    cargo build
    cd ./auto-test && python3.10 test_compiler ../target/debug/compiler --chapter 9 --stage codegen


run filename *args:
    cargo fmt
    cargo run -- {{filename}} {{args}}

test:
    cargo fmt
    cargo test -- --nocapture

vscode_config:
    #!/usr/bin/env sh
    cd haiku-vscode
    vsce package
    codium --install-extension haiku-0.0.1.vsix --force