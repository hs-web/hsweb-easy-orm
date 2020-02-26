package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.Getter;
import lombok.Setter;
import org.hswebframework.ezorm.core.Decoder;
import org.hswebframework.ezorm.rdb.codec.JdbcResultDecoder;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Predicate;
import java.util.function.Supplier;

@SuppressWarnings("all")
public class MapResultWrapper extends AbstractMapResultWrapper<Map<String, Object>> {

    @Getter
    @Setter
    private Supplier<Map<String, Object>> mapBuilder = () -> new LinkedHashMap<String, Object>();

    private static final MapResultWrapper DEFAULT_INSTANCE = new MapResultWrapper() {
        @Override
        public Map<String, Object> getResult() {
            throw new UnsupportedOperationException();
        }
    };

    public static MapResultWrapper defaultInstance() {
        return DEFAULT_INSTANCE;
    }

    @Override
    public Map<String, Object> newRowInstance() {
        return mapBuilder.get();
    }


}
