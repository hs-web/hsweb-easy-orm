package org.hswebframework.ezorm.rdb.meta.expand;

import java.util.Map;
import java.util.function.Consumer;

/**
 * @author zhouhao
 */
public class WrapperConsumer extends SimpleMapWrapper {

    private Consumer<Map<String, Object>> resultConsumer;

    public WrapperConsumer(Consumer<Map<String, Object>> resultConsumer) {
        this.resultConsumer = resultConsumer;
    }

    @Override
    public boolean done(Map<String, Object> instance) {
        resultConsumer.accept(instance);
        return false;
    }
}
