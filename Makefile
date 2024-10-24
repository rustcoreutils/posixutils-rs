#
# Copyright (c) 2024 fox0
#
# This file is part of the posixutils-rs project covered under
# the MIT License.  For the full license text, please see the LICENSE
# file in the root directory of this project.
# SPDX-License-Identifier: MIT
#

PROJECT_NAME = posixutils-rs

SRCS := $(shell find . -name '*.rs' -not -path './target/*' -not -path './*/tests/*')
POS := $(shell find locale -name '*.po')
MOS := $(POS:.po=.mo)

locale: $(MOS)

%.mo: %.po locale/${PROJECT_NAME}.pot
	msgmerge $^ -o $<
	msgfmt $< -o $@

locale/${PROJECT_NAME}.pot: $(SRCS)
	cargo r --release --bin xgettext -- -n -p locale -d ${PROJECT_NAME} $^

clean:
	rm -rf locale/${PROJECT_NAME}.pot
	rm -rf $(MOS)
