# SPDX-License-Identifier: MIT

PROJECT_NAME = posixutils-rs

SRCS := $(shell find . -name '*.rs' -not -path './target/*' -not -path './*/tests/*')
# Translation catalogs only (under LC_MESSAGES); the generated template lives at
# locale/${PROJECT_NAME}.po and must not be picked up here.
POS := $(shell find locale -path '*/LC_MESSAGES/*.po')
MOS := $(POS:.po=.mo)

locale: $(MOS)

%.mo: %.po locale/${PROJECT_NAME}.po
	msgmerge $^ -o $<
	msgfmt $< -o $@

# xgettext writes the extracted template as ${PROJECT_NAME}.po (POSIX names the
# output file <default-domain>.po, not .pot).
locale/${PROJECT_NAME}.po: $(SRCS)
	cargo r --release --bin xgettext -- -n -p locale -d ${PROJECT_NAME} $^

clean:
	rm -rf locale/${PROJECT_NAME}.po
	rm -rf $(MOS)
