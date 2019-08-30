package org.hswebframework.ezorm.rdb.utils;

import java.util.*;

public class RDBUtils {


    @SuppressWarnings("all")
    public static List<Object> convertList(Object object) {
        if (object == null) {
            return Collections.emptyList();
        }
        if (object instanceof Object[]) {
            return Arrays.asList(((Object[]) object));
        }
        if (object instanceof Collection) {
            return new ArrayList<>(((Collection) object));
        }

        return Arrays.asList(object);
    }

}
