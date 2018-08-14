package util

import (
	"strconv"

	"github.com/spf13/cobra"
)

func Gt(a interface{}, b interface{}) bool {
	return cobra.Gt(a, b)
}

func Lt(a interface{}, b interface{}) bool {
	return cobra.Gt(b, a)
}

func GtStr(a string, b string) bool {
	return Gt(a, b)
}

func LtStr(a string, b string) bool {
	return Lt(a, b)
}

func IsStrInt(s string) bool {
	if _, err := strconv.Atoi(s); err == nil {
		return true
	}
	return false
}
