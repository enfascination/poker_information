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
	echo "read filename from output and change it to the form PS0100pokerhandrank.csv, and move it to ~/projecto_staid/poker_information/"
	echo "Data is now stretch to one player in one street in one hand per row"

fromHandStreetPlayer:
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS1000pokerhandrank.csv -o distrPS1000
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0050pokerhandrank.csv -o distrPS0050
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0100pokerhandrank.csv -o distrPS0100
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0200pokerhandrank.csv -o distrPS0200
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0400pokerhandrank.csv -o distrPS0400
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0600pokerhandrank.csv -o distrPS0600
	#./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandrank_sub.csv -o distrPS0025
	echo special treatment for 25 cent blinds, bc hella
	cd ~/projecto_staid/poker_information/
	gzcat PS0025pokerhandrank.csv.gz | split -b 1700m - PS0025pokerhandranksub 
	cd -
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandranksubaa -o distrPS0025_1
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandranksubab -o distrPS0025_2
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandranksubac -o distrPS0025_3
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandranksubad -o distrPS0025_4
	cat distrPS0025_1.csv distrPS0025_2.csv distrPS0025_3.csv distrPS0025_4.csv > distrPS0025_full.csv
	echo get rid of headers from all but the first component of distrPS0025.csv
	rm ~/projecto_staid/poker_information/PS0025pokerhandranksuba*
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
	./buildpokerdistribution020.r --tag=actions_shownhands --reps=500 --quiet --version=$(version)
	./buildpokerdistribution020.r --tag=actions_unordered --reps=500 --quiet --version=$(version)

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
	mkdir ~/lib
	mkdir ~/lib/RPackages
	export R_LIBS_USER="~sfrey/lib/RPackages"
	git clone https://github.com/enfascination/poker_information.git .
	cp local_settings.r.sample local_settings.r
	cat "pathLocal <- '~/research_projects/poker_information'" >> local_settings.r
	cat "pathData <- '~/project_staid/poker_information'" >> local_settings.r
	cat "pathLib <- '~/lib/RPackages'" >> local_settings.r
	echo "now copy data"
	### scp ~/projecto_staid/poker_information/PS0025pokerhandrank.csv.gz sfrey@discovery.dartmouth.edu:~/projecto_staid/poker_information/
	### scp ~/projecto/research_projects/poker_information/distrPS*0.csv sfrey@discovery.dartmouth.edu:~/research_projects/poker_information/
	cd project_staid/poker_information
	gunzip PS0025pokerhandrank.csv.gz
	echo now fix last line of that file
	cd research_projects/poker_information
	./cluster_setup.r

discoveryStartConsole:
	qsub -N 0025_blinds_large -q default -l nodes=1:ppn=1 -feature="cellf" -l walltime=72:00:00 -M seth.frey@dartmouth.edu -m bea make discoveryStart

discoveryInteractive:
	qsub -q default -I -l feature="cellf" -l nodes=1:ppn=1 -l walltime=72:00:00

discoveryStart:
	export R_LIBS_USER="~sfrey/lib/RPackages"
	#./buildpokerdistribution020.r --tag=test --reps=1 --version=0 --quiet
	#./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0600pokerhandrank.csv -o distrPS0600
	./pokerstatedistribution006.r -f ~/projecto_staid/poker_information/PS0025pokerhandrank.csv -o distrPS0025



discoveryRemove:
	rm -rf projecto_staid/poker_information
	rm -rf research_projects/poker_information

polarisSetup:
	#mkdir /scratch/scratch-ssd/f002mmt/poker_information
	#move files to scratch-ssd/f002mmt/poker_information
	#write paths to 
	mkdir research_projects
	mkdir research_projects/poker_information
	mkdir ~/lib
	mkdir ~/lib/RPackages
	#add to bashrc:
		screen
		export R_LIBS_USER="~sfrey/lib/RPackages"
	git clone https://github.com/enfascination/poker_information.git .
	cp local_settings.r.sample local_settings.r
	cat "pathLocal <- '~/research_projects/poker_information'" >> local_settings.r
	#edit: cat "pathData <- '~/project_staid/poker_information'" >> local_settings.r
	echo "now copy data"
	#    copy to /scratch-ssid/f002mmt
	#    maybe use rsync if it works for samba:
	#       https://stackoverflow.com/questions/30994008/copy-directory-from-another-computer-on-linux
	#       rsync -uav srchost:/path/to/dir /path/to/dest
	cd project_staid/poker_information
	gunzip PS0025pokerhandrank.csv.gz
	echo now fix last line of that file
	cd research_projects/poker_information
	./cluster_setup.r
	#make ln -s to scratch

polarisStartChug0025:
	./pokerstatedistribution006.r -f /scratch-ssd/f002mmt/PS0025pokerhandrank.csv -o distrPS0025_whole
polarisStartDists:
	./buildpokerdistribution020.r --tag=test --reps=1 --version=0 --quiet

