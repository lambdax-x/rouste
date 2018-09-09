pub mod utils;

/// Execute a callback after removing unnamed segments.
#[macro_export]
macro_rules! clean_and_callback {
    (@internal $callback:ident [] [] [$($query:tt)*]) => {
        $callback($($query),*)
    };

    (@internal $callback:ident [$($acc:tt)+] [] [$($query:tt)*]) => {{
        $callback($($acc),+ $(, $query)*)
    }};

    (@internal $callback:ident [$($acc:tt)*] [($name:tt : $type:ty) $($tail:tt)*] [$($query:tt)*]) => {
        clean_and_callback!(@internal $callback [$($acc)* $name] [$($tail)*] [$($query)*])
    };

    (@internal $callback:ident [$($acc:tt)*] [$name:tt $($tail:tt)*] [$($query:tt)*]) => {
        clean_and_callback!(@internal $callback [$($acc)*] [$($tail)*] [$($query)*])
    };
    
    ($callback:ident [$($segment:tt)*] [$($query:tt)*]) => {
        clean_and_callback!(@internal $callback [] [$($segment)*] [$($query)*])
    }
}

#[macro_export]
/// Build a route from a pattern and a callback
macro_rules! route {
    (/ => $callback:ident) => {|uri: &str|
        route!(@try $callback [] [] uri)
    };

    (/?$($expected_query:tt)+ => $callback:ident) => {|uri: &str|
        route!(@try $callback [] [$($expected_query)*] uri)
    };

    (/$($expected_segment:tt)/+ => $callback:ident) => {|uri: &str| {
        route!(@try $callback [$($expected_segment)*] [] uri)
    }};

    (/$($expected_segment:tt)/+?$($expected_query:tt)&+ => $callback: ident) => {|uri: &str| {
        route!(@try $callback [$($expected_segment)*] [$($expected_query)*] uri)
    }};

    (@try $callback:ident [$($expected_segment:tt)*] [$($expected_query:tt)*] $uri:ident) => {{
        let path_query: Vec<&str> = $uri.splitn(2, '?').collect();
        path_query.get(0).and_then(|path| {
            let query_string = path_query.get(1).unwrap_or(&"");

            let segments = route!(@parse_segments [$($expected_segment)*] path);
            let queries = route!(@parse_queries [$($expected_query)*] query_string);

            match (segments, queries) {
                (($(Some(route!(@segment_pattern $expected_segment)),)* true,), ($(route!(@query_name $expected_query),)*)) => { 
                    Some(clean_and_callback!($callback [$( $expected_segment )*] [$( (route!(@query_name $expected_query)) )*]))
                },
                _ => None
            }
        })
    }};

    (@parse_segments [$($expected_segment:tt)*] $path:ident) => {{
        let segments: Vec<&str> = $path.split_terminator('/').collect();
        let mut segments_iter = segments.iter().skip(1);

        // Tuple of parsed values. Set the last element of the tuple to a boolean that indicates
        // that all the segments have been handled.
        ($({
            segments_iter.next().and_then(|&segment| {
                route!(@parse_segment $expected_segment segment)
            })
        },)* segments_iter.next().is_none(),)
    }};

    (@parse_segment ($name:tt : $type:ty) $data:expr) => {{
        percent_decode($data).and_then(|segment| segment.parse::<$type>().ok())
    }};

    (@parse_segment $name:tt $data:ident) => {{
        match $data { 
            stringify!($name) => Some(()),
            _ => None
        }
    }};

    (@segment_pattern ($name:tt : $type:ty)) => { $name };
    (@segment_pattern $name:tt) => { _ };

    (@parse_queries [$($expected_query:tt)*] $query_string:ident) => {{
        let queries: Vec<&str> = $query_string.split('&').collect();
        
        let mut key_values: ($(Option<route!(@query_type $expected_query)>,)*) = Default::default();

        for query in queries {
            if query.is_empty() {
                continue;
            }
            let ($(route!(@query_name $expected_query),)*) = key_values;
            key_values = ($(
                    route!(@query_name $expected_query).or(route!(@parse_query $expected_query query))
            ,)*);
        }

        key_values
    }};

    (@query_name ($name:tt : $type:ty)) => { $name };
    (@query_name $name:tt) => { $name };

    (@query_type ($name:tt : $type:ty)) => { $type };
    (@query_type $name:tt) => { () };

    (@parse_query ($name:tt : $type:ty) $data:ident) => {{
        let key_value: Vec<&str> = $data.splitn(2, '=').collect();
        match key_value.get(0) {
            Some(&stringify!($name)) => key_value.get(1).and_then(|value| value.parse::<$type>().ok()),
            _ => None
        }
    }};

    (@parse_query $name:tt $data:ident) => {
        match $data {
            stringify!($name) => Some(()),
            _ => None
        }
    }
}

#[macro_export]
/// Compose a router given an URI and a list of routes.
macro_rules! route_with {
    ([$($route:expr),*]) => {|uri| {
        None$(.or($route(uri)))*
    }}
}

#[cfg(test)]
mod tests {
    use utils::percent_decode;

    #[test]
    fn router() {
        
        let router = route_with!([ route!(/ => default)
                                 , route!(/double/(value: u32) => double)
                                 , route!(/triple_add_decrement/(value: u32)?(add: u32)&decrement => triple_add_decrement)
                                 , route!(/count/spaces/(data: String) => count_spaces)
        ]);

        assert_eq!(router(""), Some(1));
        assert_eq!(router("?"), Some(1));
        assert_eq!(router("/"), Some(1));
        assert_eq!(router("/?"), Some(1));
        assert_eq!(router("/?useless_stuff"), Some(1));
        assert_eq!(router("/foo"), None);

        assert_eq!(router("//"), None);

        assert_eq!(router("/double/16"), Some(32));
        assert_eq!(router("/double/wat"), None);
        assert_eq!(router("/double/16?"), Some(32));
        assert_eq!(router("/double/10?the_number=666&useless_stuff"), Some(20));

        assert_eq!(router("/triple_add_decrement/2"), Some(6));
        assert_eq!(router("/triple_add_decrement/2?"), Some(6));
        assert_eq!(router("/triple_add_decrement/2?add=4"), Some(10));
        assert_eq!(router("/triple_add_decrement/2?add=4&decrement"), Some(9));

        assert_eq!(router("/count/spaces/Church is%20Great and%20Barendregt is his Prophet%21"), Some(7));
        assert_eq!(router("/count/spaces/invalid%"), None);
    }

    fn default() -> u32 {
        1
    }

    fn double(value: u32) -> u32 {
        value * 2
    }

    fn triple_add_decrement(value: u32, add: Option<u32>, decrement: Option<()>) -> u32 {
        value * 3 + add.unwrap_or(0) - decrement.map(|_| 1).unwrap_or(0)
    }

    fn count_spaces(data: String) -> u32 {
        data.chars().filter(|&c| c == ' ').count() as u32
    }
}
