GARBAGE=.lsp-session-* .persistent-scratch history places projectile-* recentf transient/ games/ auto-save-list/ vimish-fold/
DANGEROUS=./elpa ./snippets vimish-fold/ ./custom-vars.el

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
	$(call print_in_color,$(GREEN),Clean-up successful ☑.\n)

clean:
	$(call print_in_color,$(CYAN),\n→  Purging garbage files...\n)
	@printf "$(RED)"
	@echo $(GARBAGE) | tr ' ' '\n' | xargs -I {} echo "  "⌦"  "{}
	@rm -rf $(GARBAGE)

hard_reset:
	$(call print_in_color,$(RED),\n→  HARD REST: purging ALL packages...\n)
	@printf "$(RED)"
	@echo $(GARBAGE) | tr ' ' '\n' | xargs -I {} echo "  "⌦"  "{}
	@rm -rf $(GARBAGE) $(DANGEROUS)
