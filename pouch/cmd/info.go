package cmd

import (
	"fmt"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var infoCmd = &cobra.Command{
	Use:   "info",
	Short: "Information about pouch",
	Long:  "Show the information about pouch",
	Run: func(cmd *cobra.Command, args []string) {
		showInfo()
	},
}

func init() {
	RootCmd.AddCommand(infoCmd)
}

func showInfo() {
	fmt.Println("pouch_config_file:", viper.ConfigFileUsed())
	fmt.Println("settings:")
	for k, v := range viper.AllSettings() {
		fmt.Printf("  %v: %v\n", k, v)
	}
}
