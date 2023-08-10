#!/usr/bin/env bash

commit() {
	git a .
	git commit --no-verify -m "WIP"
}

changed() {
	git status --porcelain | grep -v " tests/" | cut -d " " -f 3
}

revert() {
	revertable=$(changed)
	if [[ $revertable ]]; then
		git restore $revertable
		touch revert.log
	fi
}

tcr-run() {
	lastModificationSeconds=$(date -r revert.log +%s)
	currentSeconds=$(date +%s)
	((elapsedSeconds = currentSeconds - lastModificationSeconds))
	if ((elapsedSeconds > 1)); then
		clear
		just test && commit || revert
	fi
}

export -f tcr-run
export -f changed
export -f commit
export -f revert

watchexec -e elm tcr-run
