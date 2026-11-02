package org.hswebframework.ezorm.rdb.supports.json;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class JsonPathUtils {

    private JsonPathUtils() {
    }

    public static String normalize(Object path) {
        if (path == null) {
            return "$";
        }
        String str = String.valueOf(path).trim();
        if (str.isEmpty() || "$".equals(str)) {
            return "$";
        }
        if (str.startsWith("$")) {
            return str;
        }
        if (str.startsWith(".")) {
            return "$" + str;
        }
        if (str.startsWith("[")) {
            return "$" + str;
        }
        return "$." + str;
    }

    public static List<String> segments(Object path) {
        String normalized = normalize(path);
        if ("$".equals(normalized)) {
            return Collections.emptyList();
        }
        List<String> segments = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        int index = normalized.startsWith("$") ? 1 : 0;
        while (index < normalized.length()) {
            char ch = normalized.charAt(index);
            if (ch == '.') {
                add(segments, current);
                index++;
                continue;
            }
            if (ch == '[') {
                add(segments, current);
                int end = normalized.indexOf(']', index);
                if (end < 0) {
                    current.append(ch);
                    index++;
                    continue;
                }
                String segment = normalized.substring(index + 1, end).trim();
                if ((segment.startsWith("'") && segment.endsWith("'")) ||
                    (segment.startsWith("\"") && segment.endsWith("\""))) {
                    segment = segment.substring(1, segment.length() - 1);
                }
                if (!segment.isEmpty()) {
                    segments.add(segment);
                }
                index = end + 1;
                continue;
            }
            current.append(ch);
            index++;
        }
        add(segments, current);
        return segments;
    }

    private static void add(List<String> segments, StringBuilder current) {
        if (current.length() > 0) {
            segments.add(current.toString());
            current.setLength(0);
        }
    }
}
