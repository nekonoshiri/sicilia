package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	homedir "github.com/mitchellh/go-homedir"
	"github.com/nekonoshiri/pouch/cmd"
	"github.com/spf13/viper"
)

// go build -ldflags "-X main.VARNAME=YOUR_OWN_VALUE"
// you can use "~" as home directory
var (
	envname_POUCH_HOME        = "POUCH_HOME"
	envname_POUCH_CONFIG_NAME = "POUCH_CONFIG_NAME"
	default_POUCH_HOME        = "~"
	default_POUCH_CONFIG_NAME = ".pouch"

	default_pouchinfopath = filepath.Join("~", ".pouchinfo")
	default_startkey      = 0
	default_permission    = 0644
)

func initDefault() {
	viper.SetDefault("pouchinfopath", default_pouchinfopath)
	viper.SetDefault("startkey", default_startkey)
	viper.SetDefault("permission", default_permission)
}

func initConfigFile() {
	pouchHome := os.Getenv(envname_POUCH_HOME)
	configName := os.Getenv(envname_POUCH_CONFIG_NAME)

	if pouchHome == "" {
		pouchHome = default_POUCH_HOME
	}

	pouchHome, err := homedir.Expand(pouchHome)
	if err != nil {
		log.Fatalln("Error:", err)
	}

	if configName == "" {
		configName = default_POUCH_CONFIG_NAME
	}

	viper.AddConfigPath(pouchHome)
	viper.SetConfigName(configName)

	if err = viper.ReadInConfig(); err != nil {
		fmt.Println("Could not read config file, using default.")
	}
}

func init() {
	initConfigFile()
	initDefault()
}

func main() {
	cmd.RootCmd.Execute()
}
