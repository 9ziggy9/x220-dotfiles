GARBAGE=.lsp-session-* .persistent-scratch history places projectile-* recentf transient/ games/

# COLOR ALIASES
RED=\033[31m
GREEN=\033[32m
YELLOW=\033[33m
BLUE=\033[34m
MAGENTA=\033[35m
CYAN=\033[36m
RESET=\033[0m

# Colored output function
define print_in_color
	@printf "$1"
	@printf "$2"
	@printf "\033[0m"
endef

all: clean
	$(call print_in_color, $(GREEN), Clean-up successful.)

clean:
	rm -rf $(GARBAGE)
