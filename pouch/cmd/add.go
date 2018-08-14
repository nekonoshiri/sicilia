package cmd

import (
	"log"

	"github.com/nekonoshiri/pouch/cmd/pouch"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var addCmd = &cobra.Command{
	Use:   "add value1 value2 ...",
	Short: "Add items in pouch",
	Long:  "Add adds items in pouch.",
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) < 1 {
			log.Fatalln("Error: add needs value. See `pouch help add`")
		}
		add(args)
	},
}

func init() {
	RootCmd.AddCommand(addCmd)
}

func add(values []string) {
	startKey := viper.GetInt("startkey")
	if err := pouch.AddByEmptyIntKeys(startKey, values); err != nil {
		log.Fatalln("Error:", err)
	}
}
