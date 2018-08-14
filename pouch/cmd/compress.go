package cmd

import (
	"log"

	"github.com/nekonoshiri/pouch/cmd/pouch"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var compressCmd = &cobra.Command{
	Use:   "compress",
	Short: "Compress items in pouch",
	Long: `Move the items over to compress
[1] A, [4] B, [10] C   ->   [1] A, [2] B, [3] C
	`,
	Run: func(cmd *cobra.Command, args []string) {
		compress()
	},
}

func init() {
	RootCmd.AddCommand(compressCmd)
}

func compress() {
	startKey := viper.GetInt("startkey")
	if err := pouch.Compress(startKey); err != nil {
		log.Fatalln("Error:", err)
	}
}
