### from http://zmjones.com/make/
### and  http://blog.kaggle.com/2012/10/15/make-for-data-scientists/

#all: data model paper
#model: model.Rout
#paper: plot.Rout paper.pdf
all: prep
prep: fromHH fromhandStreetPlayer fromHandStreet fromDist misc image


fromHH:
	echo "Do more of this: this is just a demo"
	time python2.6 -i HH2csv.py ./4/*
	echo "read filename from output and change it to the form PS0100pokerhandrank.csv, and move it to /Users/sfrey/projecto_staid/poker_information/"
	echo "Data is now stretch to one player in one street in one hand per row"

fromHandStreetPlayer:
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS1000pokerhandrank.csv -o distrPS1000
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0050pokerhandrank.csv -o distrPS0050
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0100pokerhandrank.csv -o distrPS0100
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0200pokerhandrank.csv -o distrPS0200
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0400pokerhandrank.csv -o distrPS0400
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0600pokerhandrank.csv -o distrPS0600
	#./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandrank_sub.csv -o distrPS0025
	echo special treatment for 25 cent blinds, bc hella
	cd /Users/sfrey/projecto_staid/poker_information/
	gzcat PS0025pokerhandrank.csv.gz | split -b 1700m - PS0025pokerhandranksub 
	cd -
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandranksubaa -o distrPS0025_1
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandranksubab -o distrPS0025_2
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandranksubac -o distrPS0025_3
	./pokerstatedistribution006.r -f /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandranksubad -o distrPS0025_4
	cat distrPS0025_*.csv > distrPS0025.csv
	rm /Users/sfrey/projecto_staid/poker_information/PS0025pokerhandranksuba*
	rm distrPS0025_*.csv
	echo "Data is now compressed to one street in one hand per row"

discoveryRep:
	make fromHandStreetBig

version = 62

fromHandStreet:
	#./buildpokerdistribution020.r --tag=hash_wagers --reps=50 --version=$(version)
	./buildpokerdistribution020.r --tag=hash_actions --reps=50 --version=$(version)
	./buildpokerdistribution020.r --tag=actions_unordered --reps=50 --version=$(version)
	./buildpokerdistribution020.r --tag=actions_shownhands --reps=50 --version=$(version)

fromHandStreetBig:
	#./buildpokerdistribution020.r --tag=hash_wagers --reps=500 --quiet --version=$(version)
	./buildpokerdistribution020.r --tag=hash_actions --reps=500 --quiet --version=$(version)
	./buildpokerdistribution020.r --tag=actions_unordered --reps=500 --quiet --version=$(version)
	./buildpokerdistribution020.r --tag=actions_shownhands --reps=500 --quiet --version=$(version)

fromHandStreetTest:
	./buildpokerdistribution020.r --tag=test --reps=1 --version=0 --quiet
	./pokergraphs020.r -v 0

image:
	./pokergraphs020.r -v $(version)
	R CMD BATCH pokergraphs_supplementaryinformation010.r

misc:
	echo "get supporting stats"
	./pokermiscstatsfromhandcsv002.r
	echo "also get player countrs by processing all files like distrPS0025playercounts.csv, possibly in this file:pokermisstatsfromplayercsv001"
	./pokermisstatsfromplayercsv001.r

discoverySetup:
	mkdir research_projects
	mkdir research_projects/poker_information
	mkdir project_staid
	mkdir project_staid/poker_information
	echo "now copy data over from staid"
	### scp ~/projecto_staid/poker_information/PS0025pokerhandrank.csv.gz sfrey@discovery.dartmouth.edu
	### scp ~/projecto_staid/poker_information/PS0025pokerhandrank.csv.gz sfrey@discovery.dartmouth.edu
	cd project_staid/poker_information
	gunzip PS0025pokerhandrank.csv.gz
	cd research_projects/poker_information
	git clone https://github.com/enfascination/poker_information.git .
	cp local_settings.r.sample local_settings.r
	cat "pathLocal <- '~/research_projects/poker_information'" >> local_settings.r
	cat "pathData <- '~/project_staid/poker_information'" >> local_settings.r
	mkdir ~/lib
	mkdir ~/lib/RPackages
	export R_LIBS_USER="~sfrey/lib/RPackages"
	./cluster_setup.r

discoveryStart:
	module load R
	export R_LIBS_USER="~sfrey/lib/RPackages"
	./buildpokerdistribution020.r --tag=test --reps=1 --version=0 --quiet
	#./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0600pokerhandrank.csv -o distrPS0600

discoveryRemove:
	rm -rf projecto_staid/poker_information
	rm -rf research_projects/poker_information
