from lizard import FileAnalyzer, get_extensions


def nesting_depth(filename, source):
    functions = FileAnalyzer(get_extensions(['nd'])).analyze_source_code(filename, source).function_list
    return [(function.name, function.max_nesting_depth) for function in functions]


def test_rust_conditions_keep_one_logical_level_across_expression_blocks():
    source = """\
pub struct Job { pub ready: bool }

pub fn ready(online: bool, enabled: bool, cached: bool, force: bool) -> bool {
    if online && enabled || cached && force { return true; }
    false
}
pub fn with_block(online: bool, enabled: bool, cached: bool) -> bool {
    if online && { let job = Job { ready: enabled }; job.ready } && cached {
        if enabled { return true; }
    }
    false
}
pub fn with_callback(online: bool, jobs: &[Job], cached: bool, force: bool) -> bool {
    if online && jobs.iter().any(|job| {
        if cached { return true; }
        job.ready
    }) && !cached || force {
        return true;
    }
    false
}
pub fn with_body(eligible: bool, cached: bool, online: bool, blocked: bool) -> bool {
    if eligible && cached {
        return online && !blocked;
    }
    false
}
pub fn while_ready(online: bool, enabled: bool, cached: bool, force: bool) {
    while online && (enabled || cached) && !force { break; }
}

pub enum Status { Running { ready: bool }, Finished }

pub fn with_pattern(online: bool, job: Status, cached: bool) -> bool {
    if let Status::Running { ready } = job && online && ready && !cached {
        return true;
    }
    false
}
pub fn with_match(online: bool, status: Status, cached: bool) -> bool {
    if online && match status {
        Status::Running { ready } => ready,
        Status::Finished => false,
    } && cached {
        if online { return true; }
    }
    false
}
pub fn with_nested_condition(online: bool, cached: bool, force: bool) -> bool {
    if (if online { cached } else { force }) && online && !force {
        if cached { return true; }
    }
    false
}
pub fn check_struct(online: bool, ready: bool, cached: bool) -> bool {
    if (Job { ready: online && ready && cached }).ready { return true; }
    false
}
pub fn struct_before_body(online: bool, ready: bool, cached: bool) -> bool {
    if (Job { ready: online && ready && cached }).ready {
        if online { return true; }
    }
    false
}
pub fn check_tail(online: bool, ready: bool, cached: bool) -> bool {
    if { let active = ready || !cached; online && active && cached } { return true; }
    false
}
pub fn tail_before_body(online: bool, ready: bool, cached: bool) -> bool {
    if { let active = ready || !cached; online && active && cached } {
        if online { return true; }
    }
    false
}
"""
    assert nesting_depth('jobs.rs', source) == [
        ('ready', 2), ('with_block', 3), ('with_callback', 3),
        ('with_body', 3), ('while_ready', 2), ('with_pattern', 2),
        ('with_match', 3), ('with_nested_condition', 3),
        ('check_struct', 2), ('struct_before_body', 3),
        ('check_tail', 2), ('tail_before_body', 3),
    ]


def test_rust_value_blocks_keep_the_final_condition_level():
    source = """\
pub fn unsafe_condition(flag: *const bool, enabled: bool, stop: bool) -> bool {
    if unsafe { *flag && enabled } {
        if stop { return true; }
    }
    false
}

pub fn grouped_condition(online: bool, enabled: bool, stop: bool) -> bool {
    if ({ let ready = online && enabled; ready && online }) {
        if stop { return true; }
    }
    false
}

pub fn grouped_while(online: bool, enabled: bool, stop: bool) {
    while (({ online && enabled })) {
        if stop { break; }
    }
}

pub fn unsafe_then_test(flag: *const bool, enabled: bool, stop: bool) -> bool {
    if unsafe { *flag && enabled } && enabled {
        if stop { return true; }
    }
    false
}

fn evaluate(ready: impl Fn() -> bool) -> bool { ready() }

pub fn callback_predicate(flag: *const bool, enabled: bool, stop: bool) -> bool {
    if evaluate(|| unsafe { *flag && enabled }) {
        if stop { return true; }
    }
    false
}
"""
    assert nesting_depth('jobs.rs', source) == [
        ('unsafe_condition', 3), ('grouped_condition', 3), ('grouped_while', 3),
        ('unsafe_then_test', 3), ('evaluate', 0), ('callback_predicate', 2),
    ]


def test_swift_conditions_keep_one_logical_level_across_closures():
    source = """\
struct Job { let ready: Bool }

func ready(_ online: Bool, _ enabled: Bool, _ cached: Bool, _ force: Bool) -> Bool {
    if online && enabled || cached && force { return true }
    return false
}
func withClosure(_ online: Bool, _ jobs: [Job], _ cached: Bool) -> Bool {
    if online && jobs.contains { job in
        job.ready
    } && !cached {
        if cached { return true }
    }
    return false
}
func withCallback(_ online: Bool, _ jobs: [Job], _ cached: Bool, _ force: Bool) -> Bool {
    if online && jobs.contains(where: { job in
        if cached { return true }
        return job.ready
    }) && !cached || force {
        return true
    }
    return false
}
func withBody(_ eligible: Bool, _ cached: Bool, _ online: Bool, _ blocked: Bool) -> Bool {
    if eligible && cached {
        return online && !blocked
    }
    return false
}
func whileReady(_ online: Bool, _ enabled: Bool, _ cached: Bool, _ force: Bool) {
    while online && (enabled || cached) && !force { break }
}
func separateStatement(_ a: Bool, _ b: Bool, _ c: Bool, _ d: Bool, _ ping: () -> Void) {
    if a && b { ping() }
    (ping)()
    if c && d { ping() }
}
func standaloneBeforeCondition(_ waiting: Bool, _ online: Bool, _ cached: Bool) -> Bool {
    assert(waiting && online)
    if waiting {
        if [cached].contains(where: { $0 }) {
            if online { return true }
        }
    }
    return false
}
extension Job {
    func `repeat`(_ action: () -> Void) { action() }
}
func afterMember(_ job: Job, _ online: Bool, _ cached: Bool, _ ready: Bool) {
    job.repeat {}
    while online && cached && ready { break }
}
"""
    assert nesting_depth('Jobs.swift', source) == [
        ('ready', 2), ('withClosure', 3), ('withCallback', 3),
        ('withBody', 3), ('whileReady', 2), ('separateStatement', 2),
        ('standaloneBeforeCondition', 3), ('`repeat`', 0), ('afterMember', 2),
    ]


def test_rust_unary_and_let_blocks_keep_the_condition_level():
    source = """\
pub fn negated_block(ready: bool, cached: bool, retry: bool) -> bool {
    if !{ ready && cached } {
        if retry { return true; }
    }
    false
}

pub fn negated_group(ready: bool, cached: bool, retry: bool) -> bool {
    if !({ ready && cached }) {
        if retry { return true; }
    }
    false
}

pub fn block_with_sibling(ready: bool, cached: bool, retry: bool) -> bool {
    if {
        let _active = ready && cached;
        let _pending = cached && retry;
        ready
    } {
        if retry { return true; }
    }
    false
}

pub fn bare_block_operand(ready: bool, cached: bool, online: bool, retry: bool) -> bool {
    if ready && { cached && online && retry } {
        if retry { return true; }
    }
    false
}

pub fn let_condition_block(ready: bool, cached: bool, retry: bool) -> bool {
    if let true = { ready && cached } {
        if retry { return true; }
    }
    false
}

pub fn label_condition(ready: bool, cached: bool, retry: bool) -> bool {
    if 'check: {
        let active = ready && cached;
        break 'check active;
    } {
        if retry { return true; }
    }
    false
}
"""
    assert nesting_depth('retry.rs', source) == [
        ('negated_block', 3), ('negated_group', 3), ('block_with_sibling', 2),
        ('bare_block_operand', 4), ('let_condition_block', 3), ('label_condition', 2),
    ]
