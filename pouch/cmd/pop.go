package cmd

import (
	"fmt"
	"log"

	"github.com/nekonoshiri/pouch/cmd/pouch"
	"github.com/spf13/cobra"
)

var (
	pop_quiet    bool
	pop_compress bool
	// pop_eval     bool
)

var popCmd = &cobra.Command{
	Use:   "pop key1 key2 ...",
	Short: "Pop items from pouch",
	Long:  "Pop pops items from pouch: delete and echo",
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) < 1 {
			log.Fatalln("Error: pop needs key. See `pouch help pop`")
		}
		pop(args)
	},
}

func init() {
	RootCmd.AddCommand(popCmd)
	popCmd.Flags().BoolVarP(&pop_quiet, "quiet", "q", false,
		"only display the values of items")
	popCmd.Flags().BoolVarP(&pop_compress, "compress", "c", false,
		"move the items over to compress after popping")
}

func pop(keys []string) {
	values, err := pouch.Pop(keys)
	if err != nil {
		log.Fatalln("Error:", err)
	}

	for i, k := range keys {
		if pop_quiet {
			fmt.Println(values[i])
		} else {
			fmt.Printf("pop: [%s] %s\n", k, values[i])
		}
	}

	if pop_compress {
		compress()
	}
}
