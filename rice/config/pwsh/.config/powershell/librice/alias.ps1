### alias.ps1 -- Common aliases

### Hash calculation
function hash-md5 {
    param($file)
    (Get-FileHash -Algorithm MD5 $file).Hash
}

function hash-sha1 {
    param($file)
    (Get-FileHash -Algorithm SHA1 $file).Hash
}

function hash-sha256 {
    param($file)
    (Get-FileHash -Algorithm SHA256 $file).Hash
}

function hash-sha512 {
    param($file)
    (Get-FileHash -Algorithm SHA512 $file).Hash
}
