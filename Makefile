ROSWELL_VERSION := v23.10.14.114
SBCL_VERSION := 2.5.10
TEST_FILE := tests/file-server-tests.lisp

install-roswell:
	@if ! which ros > /dev/null 2>&1; then \
		echo "Roswell not found. Installing..."; \
		curl -L $(ROSWELL_BASE_URL)/$(ROSWELL_VERSION)/roswell_$(subst v,,$(ROSWELL_VERSION))-1_amd64.deb --output roswell.deb; \
		sudo dpkg -i roswell.deb; \
		ros install sbcl-bin/$(SBCL_VERSION); \
		ros use sbcl-bin/$(SBCL_VERSION); \
		echo "Roswell installation complete."; \
	else \
		echo "Roswell already installed. Skipping..."; \
	fi
	touch $@

install-dependencies:
	ros install postmodern
	ros install fiveam
	ros install cl-csv
	ros install trivial-utf-8
	ros install ironclad
	ros install macnod/dc-ds
	ros install macnod/dc-time
	ros install macnod/p-log
	ros install macnod/dc-eclectic
	ros install macnod/rbac

test:
	tests/run-tests "$(TEST_FILE)"

test-ci:
	ros run -- --disable-debugger --load "$(TEST_FILE)" --quit

.PHONY: install-roswell install-dependencies test
.DEFAULT_GOAL := test
