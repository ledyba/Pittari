#! /bin/bash

cat > ${GOFILE%.go}.gen.go <<END
package ${GOPACKAGE}

import (
	"encoding/base64"
	"fmt"
)

var gitRev = "$(git log -1 | sed -e "s/<.*@.*>//g" | base64 | tr -d '[:space:]')"
var buildAt = "$(date "+%Y/%m/%d %H:%M:%S")"

// BuildAt ...
func BuildAt() string {
	return buildAt
}

// DecodeGitRev ...
func DecodeGitRev() string {
	data, err := base64.StdEncoding.DecodeString(gitRev)
	if err != nil {
		return fmt.Sprintf("<an error occured while reading git rev: %v>", err)
	}
	if len(data) == 0 {
		return "<not available>"
	}
	return string(data)
}

END
