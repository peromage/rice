file_url=$(curl -s https://api.github.com/repos/GloriousEggroll/proton-ge-custom/releases/latest | grep browser_download_url | cut -d\" -f4 | grep .tar.gz)
file_checksum_url=$(curl -s https://api.github.com/repos/GloriousEggroll/proton-ge-custom/releases/latest | grep browser_download_url | cut -d\" -f4 | grep .sha512sum)

file=${file_url##*/}
file_checksum=${file_checksum_url##*/}
install_dir=~/.steam/root/compatibilitytools.d

if [ ! -f $file ]; then
	echo "Downloading ProtonGE"
	curl -sLOJ $file_url
fi
echo "Downloaded $file"

if [ ! -f $file_checksum ]; then
	echo "Downloading ProtonGE checksum"
	curl -sLOJ $file_checksum_url
fi
echo "Downloaded $file_checksum"

if ! sha512sum -c $file_checksum; then
	echo "Checksum failed: $file_checksum"
	exit 1
fi
echo "Verified checksum: $file_checksum"

echo "Installing $file"
mkdir -p $install_dir
tar -xf $file -C $install_dir
echo "Installed $file"
echo "All done"
