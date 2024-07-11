### hash.psm1 --- Hash calculation

function hash_md5 {
    param($file)
    (Get-FileHash -Algorithm MD5 $file).Hash
}

function hash_sha1 {
    param($file)
    (Get-FileHash -Algorithm SHA1 $file).Hash
}

function hash_sha256 {
    param($file)
    (Get-FileHash -Algorithm SHA256 $file).Hash
}

function hash_sha512 {
    param($file)
    (Get-FileHash -Algorithm SHA512 $file).Hash
}

Export-ModuleMember -Function *
