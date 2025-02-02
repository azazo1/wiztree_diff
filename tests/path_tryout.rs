// 对 rust std::path 的一些尝试

use std::path::{Component, Path, PathBuf, Prefix, PrefixComponent};

#[test]
#[cfg(windows)]
fn path_components() {
    let buf = PathBuf::from("D:/a/b/c/d");
    let mut p = buf.components();
    assert!(matches!(p.next(), Some(Component::Prefix(PrefixComponent{..}))));
    assert!(matches!(p.next(), Some(Component::RootDir)));
    assert!(matches!(p.next(), Some(Component::Normal(_a))));
    assert!(matches!(p.next(), Some(Component::Normal(_b))));
    assert!(matches!(p.next(), Some(Component::Normal(_c))));
    assert!(matches!(p.next(), Some(Component::Normal(_d))));
    assert!(p.next().is_none());
}

#[test]
fn parent_path() {
    #[cfg(windows)] {
        let path: PathBuf = "D:\\".into();
        let mut it = path.components();
        assert!(matches!(it.next(), Some(Component::Prefix(PrefixComponent{..}))));
        assert_eq!(it.next(), Some(Component::RootDir));
        assert_eq!(it.next(), None);
        assert_eq!(path.parent(), None);
    }

    let empty_path = PathBuf::from("");
    assert_eq!(empty_path.components().next(), None);

    let slash = PathBuf::from("/");
    assert_eq!(slash.components().collect::<Vec<_>>(), [Component::RootDir]);
    assert_eq!(slash.parent(), None);
}

#[test]
fn prefix_path() {
    #[cfg(windows)] {
        let prefix = Path::new("D:\\").components().next().unwrap();
        let Component::Prefix(prefix) = prefix else { panic!(); };
        assert!(matches!(prefix.kind(), Prefix::Disk(68)));
        let path_from_prefix_comp = Path::new(prefix.as_os_str());
        dbg!(path_from_prefix_comp);
        assert_eq!(PathBuf::from("D:/"), PathBuf::from("D:\\"));

        let root: PathBuf = "D:/".into();
        let a: PathBuf = "D:/a".into();
        let _b: PathBuf = "D:/a/b".into();
        let b_d: PathBuf = "D:/a/b/..".into();
        let b_dd: PathBuf = "D:/a/b/../..".into();
        assert!(a.starts_with(&root));
        assert!(b_d.starts_with(&a));
        assert!(b_dd.starts_with(&a)); // 实际上, 这个应该为 false, 但是 b_dd 这个路径没有标准化
        // 路径的标准化我知道的有两种方法:
        // - 使用 fs::canonicalize, 这个还没了解过具体的情况, 还不知道其和 Path::canonicalize 的区别.
        //   两者似乎一样, 但是对 "/" 在 Windows 下进行调用会产生奇怪的值.
        // - 使用 path_clean::clean
        let b_dd_normalized = path_clean::clean(b_dd.clone());
        dbg!(&b_dd_normalized);
        assert_eq!(b_dd_normalized, Path::new("D:/"));
        assert!(!b_dd_normalized.starts_with(&a));
    }
    // 空路径
    let root: PathBuf = "/".into();
    let empty: PathBuf = Default::default();
    assert!(empty.starts_with(&empty));
    assert!(!empty.starts_with(&root));
    assert!(root.starts_with(&empty)); // 空路径满足其他路径的 starts_with
}

#[test]
fn strip_prefix_path() {
    assert_ne!(PathBuf::from("b/c/d").strip_prefix("b/c/d/").ok(), None);
    assert_eq!(PathBuf::from("b/c/d").strip_prefix("b/c/d/e").ok(), None);
}

#[test]
#[cfg(windows)]
fn path_file_name() {
    assert_eq!(PathBuf::from("D:/").file_name(), None);
}

#[test]
#[cfg(windows)]
fn path_disk_prefix() {
    let mut path = PathBuf::from("D:");
    let mut comps = path.components();
    assert!(matches!(comps.next(), Some(Component::Prefix(PrefixComponent{..}))));
    assert!(comps.next().is_none());
    path.push("/");
    let mut comps = path.components();
    assert!(matches!(comps.next(), Some(Component::Prefix(PrefixComponent{..}))));
    assert!(matches!(comps.next(), Some(Component::RootDir)));
    assert!(comps.next().is_none());
    // wrong path
    let mut path = PathBuf::from("D:");
    path.push("a");
    assert_eq!(path, PathBuf::from("D:a"))
}

#[test]
#[cfg(windows)]
fn trailing_slash_path() {
    let slashed = Path::new("D:/");
    let not_slashed = Path::new("D:");
    assert_ne!(slashed, not_slashed);
}

#[test]
#[cfg(windows)]
fn canonicalize_path() {
    use std::fs::canonicalize;
    // assert_eq!(canonicalize("/").unwrap(), PathBuf::from("\\\\?\\D:\\"));
    // 产生的路径是: "\\\\?\\D:\\", `\\?\` 这个前缀似乎是用来表示这个路径是长路径.
    assert_eq!(canonicalize("D:/").unwrap(), PathBuf::from("\\\\?\\D:\\"));
    assert_eq!(
        Path::new("D:/").canonicalize().unwrap(),
        canonicalize(Path::new("D:/")).unwrap()
    );
    assert!(
        ! // 这里就很麻烦了, 标准化后的路径是以 `\\?\` 开头的, 但是这个前缀仍然参与 starts_with 判断.
            Path::new("D:/").canonicalize().unwrap()
                .starts_with("D:/")
    );
    dbg!(Path::new("D:/").canonicalize().unwrap().components().collect::<Vec<_>>());
}

#[test]
#[cfg(windows)]
fn path_eq() {
    assert_eq!(PathBuf::from("a\\"), PathBuf::from("a"));
    assert_eq!(PathBuf::from("D:/a/b"), PathBuf::from("D:/a/b/"));
    assert_eq!(PathBuf::from("D:/a/b"), PathBuf::from("D:\\a\\b\\"));
    assert_eq!(PathBuf::from("D:\\a\\b"), PathBuf::from("D:\\a\\b\\"));
}