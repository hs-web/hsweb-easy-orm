package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.core.ValueCodec;

import java.util.LinkedList;
import java.util.List;

public class CompositeValueCodec implements ValueCodec<Object, Object> {

    private LinkedList<ValueCodec> codecs = new LinkedList<>();

    public CompositeValueCodec addCodec(ValueCodec codec) {
        codecs.add(codec);
        return this;
    }

    public CompositeValueCodec addFirst(ValueCodec codec) {
        codecs.addFirst(codec);
        return this;
    }

    public CompositeValueCodec addAllCodec(List<ValueCodec> codec) {
        codecs.addAll(codec);
        return this;
    }

    @Override
    public Object encode(Object value) {
        for (ValueCodec codec : codecs) {
            value = codec.encode(value);
        }
        return value;
    }

    @Override
    public Object decode(Object data) {

        for (ValueCodec codec : codecs) {
            data = codec.encode(data);
        }
        return data;
    }
}
