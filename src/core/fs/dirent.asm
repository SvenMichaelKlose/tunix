; Directory entry structure

dirent_name     = 0
dirent_length   = max_file_name_length
dirent_type     = @(+ dirent_length 4)
dirent_size     = @(+ dirent_type 1)
