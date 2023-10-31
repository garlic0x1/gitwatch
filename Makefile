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

default: install

build:
	ros build gitwatch.ros

install: build
	mv gitwatch $(INSTALL_DIRECTORY)/gitwatch
	gitwatch migrate

clean:
	rm $(INSTALL_DIRECTORY)/gitwatch
	rm /tmp/gitwatch.sqlite3

# end
