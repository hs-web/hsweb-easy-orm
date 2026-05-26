package org.hswebframework.ezorm.rdb.supports.json;

public interface JsonTermType {

    String exists = "json_exists";

    String notExists = "json_not_exists";

    String contains = "json_contains";

    String notContains = "json_not_contains";

    String contained = "json_contained";

    String notContained = "json_not_contained";

    String value = "json_value";
}
