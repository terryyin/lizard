// The reads still outstanding in this process, one per key: requesters of
// the same key while its read is outstanding share that one read, and the
// entry leaves once the read settles, so a failure is never kept and a later
// requester asks again. Each read is bounded from its start; what a settled
// read's answer is kept for, if anything, belongs to the caller.
//
// A read is ended either by its owner's tracked controllers (`read`), or by
// its waiters (`waitFor`): it ends once no requester waits for it, and an
// owner that closes ends it by ending every requester that waits.

// Why a read was aborted at its own bound, rather than because nobody waits
// or its owner closed.
export class ReadBoundReached extends Error {
  constructor() {
    super("The read reached its own bound.");
  }
}

/** @template T */
export class OutstandingReads {
  /** @type {Map<string, { readonly read: Promise<T>; readonly controller: AbortController; waiters: number }>} */
  #reading = new Map();

  #boundMs;

  // `boundMs` is how long one read may run from its start before it is
  // aborted with `ReadBoundReached`, unless its first requester names its
  // own.
  /** @param {number} boundMs */
  constructor(boundMs) {
    this.#boundMs = boundMs;
  }

  // The outstanding read for `key`, or else a new one that `start` begins
  // with a signal aborted at the bound or when its owner closes; `tracked`
  // is the owner's set of controllers it aborts on closing.
  /**
   * @param {string} key
   * @param {Set<AbortController>} tracked
   * @param {(signal: AbortSignal) => Promise<T>} start
   * @returns {Promise<T>}
   */
  read(key, tracked, start) {
    const outstanding = this.#reading.get(key);
    if (outstanding !== undefined) {
      return outstanding.read;
    }
    const { read, controller } = this.#started(key, start);
    tracked.add(controller);
    void read
      .finally(() => {
        tracked.delete(controller);
      })
      .catch(() => undefined);
    return read;
  }

  // One requester's wait for the read under `key`, joining the outstanding
  // one or else starting it. The wait settles with the read, or rejects with
  // `waiter`'s reason once `waiter` aborts; the read itself is aborted only
  // when its last waiter leaves.
  /**
   * @param {string} key
   * @param {AbortSignal} waiter
   * @param {(signal: AbortSignal) => Promise<T>} start
   * @param {number} [boundMs]
   * @returns {Promise<T>}
   */
  waitFor(key, waiter, start, boundMs) {
    if (waiter.aborted) {
      return Promise.reject(waiter.reason);
    }
    const outstanding =
      this.#reading.get(key) ?? this.#started(key, start, boundMs);
    outstanding.waiters += 1;
    return new Promise((resolve, reject) => {
      const leave = () => {
        outstanding.waiters -= 1;
        if (outstanding.waiters === 0) {
          this.#ended(key, outstanding);
          // Keep the waiter's reason (owned bound vs ordinary departure) so
          // the shared read can classify its end without inventing one.
          outstanding.controller.abort(waiter.reason);
        }
        reject(waiter.reason);
      };
      waiter.addEventListener("abort", leave, { once: true });
      outstanding.read
        .finally(() => {
          waiter.removeEventListener("abort", leave);
        })
        .then(resolve, reject);
    });
  }

  #started(key, start, boundMs = this.#boundMs) {
    const controller = new AbortController();
    const timer = setTimeout(() => {
      this.#ended(key, outstanding);
      controller.abort(new ReadBoundReached());
    }, boundMs);
    const outstanding = {
      read: start(controller.signal).finally(() => {
        clearTimeout(timer);
        this.#ended(key, outstanding);
      }),
      controller,
      waiters: 0,
    };
    this.#reading.set(key, outstanding);
    return outstanding;
  }

  // A read being ended is no longer outstanding: a later requester starts a
  // new one rather than joining it.
  #ended(key, outstanding) {
    if (this.#reading.get(key) === outstanding) {
      this.#reading.delete(key);
    }
  }
}
