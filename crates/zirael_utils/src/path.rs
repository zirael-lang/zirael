use std::path::PathBuf;

/// strips the same root from a path using reference_path as the common base
pub fn strip_same_root(path: PathBuf, reference_path: PathBuf) -> PathBuf {
    let path_components: Vec<_> = path.components().collect();
    let reference_components: Vec<_> = reference_path.components().collect();

    let mut common_length = 0;

    for (path_comp, ref_comp) in path_components.iter().zip(reference_components.iter()) {
        if path_comp == ref_comp {
            common_length += 1;
        } else {
            break;
        }
    }

    let stripped_path: PathBuf = path_components.iter().skip(common_length).collect();

    stripped_path
}
