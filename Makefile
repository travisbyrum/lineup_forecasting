RDIR := .
CODE_DIR := $(RDIR)/R
DATA_DIR := $(RDIR)/data
REPORTS := $(RDIR)/reports

all: $(DATA_DIR)/final_data.csv

$(DATA_DIR)/final_data.csv: $(DATA_DIR)/win_expectation.csv $(DATA_DIR)/player_stats.csv
	@echo "*** Making final_data.csv"
	@Rscript $(CODE_DIR)/lineup-combine.R $(@D)

$(REPORTS)/LineupEval.pdf: $(DATA_DIR)/final_data.csv
	@echo "*** Making final report"
	@Rscript $(CODE_DIR)/knit-report.R $(REPORTS)/LineupEval.Rnw

$(DATA_DIR)/win_expectation.csv: $(DATA_DIR)/lineups.csv
	@echo "*** Making win_expecation.csv"
	@Rscript $(CODE_DIR)/win-expectation.R $(@D)

$(DATA_DIR)/player_stats.csv:
	@echo "*** Making player_stats.csv"
	@Rscript $(CODE_DIR)/bref-scraping.R $(@D)

.PHONY: all