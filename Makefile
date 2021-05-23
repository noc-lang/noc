LOCAL_BIN_PATH:=${HOME}/.local/bin/noc
LOCAL_LIB_PATH:=${HOME}/.local/lib/noc

install:
	@echo "Building/Copying Noc binary ..."
	@stack install

	@echo "Copying Noc library ..."
	@mkdir $(LOCAL_LIB_PATH)
	@cp -R std $(LOCAL_LIB_PATH)/std

	@echo "\033[0;32m noc installation is done.\033[0m"

uninstall: 
	@echo "Removing Noc binary ..."
	@rm $(LOCAL_BIN_PATH)
	
	@echo "Remove Noc library ..."
	@rm -rf $(LOCAL_LIB_PATH)

	@echo "\033[0;32m noc uninstallation is done.\033[0m"
	