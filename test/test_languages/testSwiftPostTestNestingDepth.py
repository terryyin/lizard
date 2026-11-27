from lizard import FileAnalyzer, get_extensions


def test_repeat_while_owns_its_body_and_one_logical_level():
    source = """\
func plain(_ retry: () -> Bool, _ online: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { if retry() { break } } while online && enabled && pending
}

func grouped(_ retry: () -> Bool, _ online: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { if retry() { break } } while (online && enabled && pending)
}

func repeatBody(_ retry: () -> Bool, _ online: Bool, _ pending: Bool) {
    repeat { if retry() { if online { break } } } while pending
}

func whileBody(_ retry: () -> Bool, _ online: Bool, _ pending: Bool) {
    while pending { if retry() { if online { break } } }
}

func afterRepeat(_ retry: () -> Bool, _ online: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { if retry() { break } } while online &&
        enabled && pending
    if enabled { if online { _ = retry() } }
}

func afterTypeTest(_ run: () -> Void, _ status: Any, _ online: Bool) {
    repeat { run() } while status is Optional<Bool>
    if online { if online { run() } }
}

func withTrailingCallback(_ run: () -> Void, _ online: Bool, _ entries: [Bool]) {
    repeat { run() } while entries.contains { $0 && online } && online
    if online { if online { run() } }
}
"""
    functions = FileAnalyzer(get_extensions(['nd'])).analyze_source_code('Retry.swift', source).function_list
    assert [(function.name, function.max_nesting_depth) for function in functions] == [
        ('plain', 2), ('grouped', 2), ('repeatBody', 3), ('whileBody', 3), ('afterRepeat', 2),
        ('afterTypeTest', 2), ('withTrailingCallback', 2),
    ]


def test_repeat_tail_keeps_generic_commas_and_trailing_closure_operators():
    source = """\
func typeAndFlags(_ poll: () -> Void, _ value: Any, _ connected: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { poll() } while value is Result<Bool, any Error> && connected && enabled && pending
}

func groupedTypeAndFlags(_ poll: () -> Void, _ value: Any, _ connected: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { poll() } while (value is Result<Bool, any Error> && connected && enabled && pending)
}

func closureAndFlags(_ poll: () -> Void, _ values: [Bool], _ ready: Bool, _ enabled: Bool, _ pending: Bool) {
    repeat { poll() } while ready && values.contains {
        $0
    } && enabled && pending
}

func trailingOperator(_ poll: () -> Void, _ first: Bool, _ ready: Bool, _ active: Bool) {
    repeat { poll() } while first &&
        ready && active
    if ready { if active { poll() } }
}

func siblingOnLine(_ poll: () -> Void, _ ready: Bool, _ active: Bool) {
    repeat { poll() } while ready && active; if ready { if active { poll() } }
}

func nestedRepeat(_ poll: () -> Void, _ ready: Bool, _ active: Bool) {
    repeat {
        repeat {
            if ready { if active { break } }
        } while ready && active
    } while active && ready
}

func tailAfterClosure(_ poll: () -> Void, _ entries: [Bool], _ ready: Bool,
                      _ enabled: Bool, _ cached: Bool, _ connected: Bool) {
    repeat { poll() } while ready && entries.contains {
        $0
    } && enabled && cached && connected
}

func groupedTailAfterClosure(_ poll: () -> Void, _ entries: [Bool], _ ready: Bool,
                             _ enabled: Bool, _ cached: Bool, _ connected: Bool) {
    repeat { poll() } while (ready && entries.contains {
        $0
    } && enabled && cached && connected)
}

func multilineType(_ poll: () -> Void, _ value: Any, _ connected: Bool,
                   _ enabled: Bool, _ pending: Bool) {
    repeat { poll() } while value is Result<
        Bool,
        any Error
    > && connected && enabled && pending
    if connected { if enabled { poll() } }
}
"""
    functions = FileAnalyzer(get_extensions(['nd'])).analyze_source_code('Retry.swift', source).function_list
    assert [(function.name, function.max_nesting_depth) for function in functions] == [
        ('typeAndFlags', 2), ('groupedTypeAndFlags', 2), ('closureAndFlags', 2),
        ('trailingOperator', 2), ('siblingOnLine', 2), ('nestedRepeat', 4),
        ('tailAfterClosure', 2), ('groupedTailAfterClosure', 2), ('multilineType', 2),
    ]
