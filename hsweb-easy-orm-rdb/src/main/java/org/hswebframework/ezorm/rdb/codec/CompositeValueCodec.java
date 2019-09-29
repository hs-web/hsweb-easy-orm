package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.ezorm.core.Decoder;
import org.hswebframework.ezorm.core.Encoder;
import org.hswebframework.ezorm.core.ValueCodec;

import java.util.LinkedList;
import java.util.List;

public class CompositeValueCodec implements ValueCodec<Object, Object> {

    private LinkedList<Encoder> encoders = new LinkedList<>();
    private LinkedList<Decoder> decoders = new LinkedList<>();

    public CompositeValueCodec addEncoder(Encoder encoder){
        encoders.add(encoder);
        return this;
    }

    public CompositeValueCodec addDecoder(Decoder encoder){
        decoders.add(encoder);
        return this;
    }

    public CompositeValueCodec addEncoderFirst(Encoder encoder){
        encoders.addFirst(encoder);
        return this;
    }

    public CompositeValueCodec addDecoderFirst(Decoder encoder){
        decoders.addFirst(encoder);
        return this;
    }
    @Override
    public Object encode(Object value) {
        for (Encoder codec : encoders) {
            value = codec.encode(value);
        }
        return value;
    }

    @Override
    public Object decode(Object data) {

        for (Decoder codec : decoders) {
            data = codec.decode(data);
        }
        return data;
    }
}
