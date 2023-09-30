function b() {
    clear
    cargo build $@
}

function t() {
    clear
    cargo test $@
}

function format_all() {
    find . -name '*.rs' | xargs rustfmt
}