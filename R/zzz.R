.onLoad <- function(...){
	packageStartupMessage("\n")
	packageStartupMessage("Welcome to lwlegopt.")
	packageStartupMessage("\n")
	packageStartupMessage("Version: ",utils::packageDescription('lwlegopt')$Version)
	packageStartupMessage("\n")
	packageStartupMessage("If this is your first time running lwlegopt you should see ?Draw_lego")
	packageStartupMessage("\n")
	#packageStartupMessage("Remember to keep lwlegopt up to date: update.packages(repos=\"http://R-Forge.R-project.org\")")
	}