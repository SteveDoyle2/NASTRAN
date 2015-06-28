import os

def get_files_of_type(dirname, extensions='.txt', maxSize=100., recursive=False):
    """
    Gets the list of all the files with a given extension in the specified directory

    :param dirname:   the directory name
    :param extension: list of filetypes to get (default='.txt')
    :param maxSize:   size in MB for max file size
    :returns: list of all the files with a given extension in the specified directory
    """
    if isinstance(extensions, str):
        extensions = [extensions]
    filenames = []
    if recursive:
        for f in os.listdir(dirname):
            dirname_file = os.path.join(dirname, f)
            if f.startswith('_'):
                print('ignoring %s' % dirname_file)
                continue
            if os.path.isdir(dirname_file):
                filenames += get_files_of_type(dirname_file, extensions, recursive=recursive)
            else:
                for extension in extensions:
                    if extension in ['mod']:
                        print(f)
                    if os.path.splitext(f)[1].endswith(extension):
                        filenames.append(os.path.join(dirname, f))
                        break
        return filenames
    else:
        if not os.path.exists(dirname):
            return []
        return [os.path.join(dirname, f) for f in os.listdir(dirname)
                if os.path.splitext(f)[1].endswith(extensions[0])
                 and os.path.getsize(os.path.join(dirname, f)) / (1048576.) <= maxSize]

def main():

    dirs =[
        'alt', 'mds', 'mis', 'modmis', 'nid', 'rf', 'um',
    ]
    build_msg = ''
    all_f90_files = []
    if not os.path.exists('mod'):
        os.mkdir('mod')

    for src_dir in dirs:
        print(src_dir)
        f90_files = get_files_of_type(src_dir, ['f90', 'f'], recursive=True)
        all_f90_files += f90_files
        #print(f90_files)
        #for fname in f90_files2:
        #    build_msg += 'gfortran %s\n' % fname #  -I/mod
    f90_files2 = [fname.replace('\\', '/') for fname in all_f90_files]


    all_f90_obj_files = [fname.replace('\\', '/') for fname in all_f90_files]

    #build_msg += 'gfortran -o nosatran ' + ' '.join(all_f90_obj_files2)
    build_msg += 'gfortran -o nosatran -fno-range-check ' + ' '.join(f90_files2) + '  > nastran.log 2>&1'

    with open('build.sh', 'w') as f:
        f.write(build_msg)

if __name__ == '__main__':
    main()