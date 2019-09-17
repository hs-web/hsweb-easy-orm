package org.hswebframework.ezorm.rdb.executor;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public interface Record extends Map<String, Object> {

    Optional<Object> get(String key);

    Optional<String> getString(String key);

    Optional<Integer> getInteger(String key);

    Optional<Boolean> getBoolean(String key);

    Optional<Date> getDate(String key);

    Optional<Record> getNest(String key);

    Optional<List<Record>> getNests(String key);
}
