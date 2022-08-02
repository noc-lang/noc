LOCAL_BIN_PATH:=${HOME}/.local/bin
LOCAL_LIB_PATH:=${HOME}/.local/share

install_noc:
	@echo "Building/Copying Noc binary ..."
	@stack install

install_vm:
	@echo "Building/Copying Noc VM binary ..."
	@gcc $(shell find ./src/Language/Noc/VM -name '*.c') -o noc_vm -lm
	mv noc_vm $(LOCAL_LIB_PATH)/noc

install_std:
	@echo "Copying Noc library ..."
	@mkdir -p $(LOCAL_LIB_PATH)/noc/std
	@cp -R std $(LOCAL_LIB_PATH)/noc

install: install_noc install_std install_vm 
	@echo "\033[0;32m noc installation is done.\033[0m"

uninstall: 
	@echo "Removing Noc binary ..."
	@rm $(LOCAL_BIN_PATH)/noc
	
	@echo "Remove Noc library ..."
	@rm -rf $(LOCAL_LIB_PATH)/noc

	@echo "\033[0;32m noc uninstallation is done.\033[0m"

update: uninstall install