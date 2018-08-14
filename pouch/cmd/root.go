package cmd

import (
	"github.com/spf13/cobra"
)

var RootCmd = &cobra.Command{
	Use:   "pouch",
	Short: "nano-size command line application to push/pop items.",
	Long:  `Pouch is a nano-size command line application to push/pop items.`,
}
