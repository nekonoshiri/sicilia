package cmd

import (
	"log"

	"github.com/nekonoshiri/pouch/cmd/pouch"
	"github.com/spf13/cobra"
)

var cleanCmd = &cobra.Command{
	Use:   "clean",
	Short: "Clean all items in pouch",
	Long:  "Clean deletes all items in pouch",
	Run: func(cmd *cobra.Command, args []string) {
		if err := pouch.Clean(); err != nil {
			log.Fatalln("Error:", err)
		}
	},
}

func init() {
	RootCmd.AddCommand(cleanCmd)
}
