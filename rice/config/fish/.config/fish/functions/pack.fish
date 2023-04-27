function pack
    argparse -N 1 "d/delete" -- $argv
    or return

    if set -q _flag_d
        set remove_files --remove-files
    end
    for i in $argv
        tar $remove_files -czvf (basename $i).tar.gz $i
    end
end
