FROM fedora:34

RUN dnf groupinstall -y "Development Tools"
RUN dnf install -y ruby ruby-devel rubygems rpm-build sbcl libffi-devel redhat-rpm-config git zlib-devel
# For iolib:
RUN dnf install -y gcc-c++
# Rest of deps
RUN dnf install -y webkit2gtk3 glib-networking gsettings-desktop-schemas xclip enchant2 libfixposix libfixposix-devel
## TODO: notify-osd?

# Copy repo content inside container:
COPY . /root/nyxt
## TODO: Following is useless?
RUN cd /root/nyxt

# COPY build-scripts/build-fedora-package.sh /usr/local/bin/
# ENTRYPOINT ["build-fedora-package.sh"]

RUN /root/nyxt/build-scripts/build-fedora-package.sh
