# Makefile — incremental compile + change-driven testing for Rackton.
#
# `make test` recompiles only the modules whose source changed and runs
# only the tests whose transitive dependency closure changed since they
# last passed. Both facts come from Racket's own machinery:
#
#   * `raco make` recompiles a module only when its SHA-1 hash changed,
#     and writes a `compiled/<name>_rkt.dep` file recording that
#     module's direct dependencies.
#   * tools/gen-test-deps.rkt (over tools/gen-test-deps-lib.rkt) walks
#     those `.dep` files transitively and emits deps.mk: a `TEST_STAMPS`
#     list and, per test, the set of in-repo sources it depends on. The
#     same tool lists the source inventory (`--list-sources`), so no
#     file set or exclusion rule is repeated here.
#
# Nothing here names a test file. Discovery lives entirely in the
# generator, so a newly added `*-test.rkt` file is picked up with no
# edit to this Makefile.
#
# Targets:
#   make            compile everything (incremental)
#   make test       run only the tests whose sources changed (add -jN)
#   make test-all   run the full suite unconditionally
#   make clean      remove stamps, deps.mk, and all compiled output

STAMP_DIR     := .mk-stamps
DEPS_MK       := deps.mk
GEN           := tools/gen-test-deps.rkt
GEN_LIB       := tools/gen-test-deps-lib.rkt
COMPILE_STAMP := $(STAMP_DIR)/.compiled
JOBS          ?= $(shell nproc 2>/dev/null || echo 4)

# The source inventory comes from the generator's own directory walk,
# so the exclusion policy (compiled/, .git/, doc/) is defined once, in
# the library. Evaluated once per make invocation. `racket file.rkt`
# recompiles the generator whenever its source is newer than any cached
# .zo, which is the state a normal edit or checkout leaves behind.
RKT := $(shell racket $(GEN) --list-sources)

.PHONY: all compile test test-all clean

all: compile

compile: $(COMPILE_STAMP)

# The one place `raco make` is invoked. Incremental: only changed
# modules recompile, and every .dep the generator reads is refreshed.
$(COMPILE_STAMP): $(RKT)
	@echo "raco make -j $(JOBS) (incremental, $(words $(RKT)) sources)"
	@raco make -j $(JOBS) $(RKT)
	@mkdir -p $(STAMP_DIR)
	@touch $@

# Regenerate the dependency fragment after compilation, whenever a
# source or the generator changes.
$(DEPS_MK): $(COMPILE_STAMP) $(GEN) $(GEN_LIB)
	@racket $(GEN) > $@

# Pulls in TEST_STAMPS and the per-test prerequisite rules. If deps.mk
# is missing or stale, GNU Make builds it (above) and re-reads it before
# evaluating any goal below.
-include $(DEPS_MK)

# Run only the tests whose stamp is older than one of its prerequisites.
test: $(TEST_STAMPS)

# Recipe for every stamp; the prerequisites are supplied by deps.mk.
# $* is the test's path relative to the repo root, e.g.
# private/unify-test.rkt.
$(STAMP_DIR)/%.stamp:
	@mkdir -p $(dir $@)
	raco test $*
	@touch $@

# Escape hatch: run the whole suite regardless of change detection.
test-all:
	raco test -p rackton

clean:
	rm -rf $(STAMP_DIR) $(DEPS_MK)
	find . -type d -name compiled -prune -exec rm -rf {} +
