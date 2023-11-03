##
# gitwatch
#
#
# Dependencies:
#
# Roswell
# Quicklisp
#

INSTALL_DIRECTORY = ~/.local/bin
DB_LOCATION = ~/.local/gitwatch.sqlite3

default: install

prepare:
	touch $(DB_LOCATION)

build:
	ros build gitwatch.ros

install: build prepare
	mv gitwatch $(INSTALL_DIRECTORY)/gitwatch
	gitwatch migrate
	# Add a sample repo :)
	gitwatch repo add "https://github.com/garlic0x1/gitwatch.git"

update: build
	mv gitwatch $(INSTALL_DIRECTORY)/gitwatch

clean:
	rm $(INSTALL_DIRECTORY)/gitwatch
	rm $(DB_LOCATION)

# end
