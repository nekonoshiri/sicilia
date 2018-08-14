package cmd

import (
	"fmt"
	"log"

	"github.com/nekonoshiri/pouch/cmd/pouch"
	pu "github.com/nekonoshiri/pouch/cmd/pouch/util"
	"github.com/nekonoshiri/pouch/cmd/util"
	"github.com/spf13/cobra"
)

var list_quiet bool

var listCmd = &cobra.Command{
	Use:   "list",
	Short: "List the items in pouch",
	Long:  "List lists the items in pouch.",
	Run: func(cmd *cobra.Command, args []string) {
		list()
	},
}

func init() {
	RootCmd.AddCommand(listCmd)
	listCmd.Flags().BoolVarP(&list_quiet, "quiet", "q", false,
		"only display the values of items")
}

func list() {
	pch, err := pouch.Read()
	if err != nil {
		log.Fatalln("Error:", err)
	}
	if !list_quiet {
		fmt.Println(len(*pch), "item(s) in pouch")
	}

	pu.RangeWithKeySort(pch, util.LtStr, func(k, v string) {
		if list_quiet {
			fmt.Println(v)
		} else {
			fmt.Printf("[%s] %s\n", k, v)
		}
	})
}
