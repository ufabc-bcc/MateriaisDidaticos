BUILD_DIR=build

define compile_org
	cd $(BUILD_DIR) ; emacs --batch --eval '(load "$(PWD)/$(BUILD_DIR)/export_conf.el")' $(1).org -f org-latex-export-to-pdf -- kill
	cp $(BUILD_DIR)/$(1).pdf .
endef

.PHONY: default

default: all;

build_dir:
	mkdir -p $(BUILD_DIR)

sync_dir: build_dir
	rsync -av . $(BUILD_DIR) --exclude=$(BUILD_DIR)

l01: sync_dir
	$(call compile_org,lista01)

l02: sync_dir
	$(call compile_org,lista02)

l03: sync_dir
	$(call compile_org,lista03)

l04: sync_dir
	$(call compile_org,lista04)

l05: sync_dir
	$(call compile_org,lista05)

l06: sync_dir
	$(call compile_org,lista06)

l07: sync_dir
	$(call compile_org,lista07)

l08: sync_dir
	$(call compile_org,lista08)

all: l01 l02 l03 l04 l05 l06 l07 l08

clean:
	rm -fr build

full_clean: clean
	rm *.pdf
