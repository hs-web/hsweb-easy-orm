package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.meta.ColumnMetadata;

public interface ValueCodec<E, D> extends Encoder<E>, Decoder<D> {

    default E encodeNull(){
        return null;
    }

    E encode(Object value);

    D decode(Object data);

    default E encode(Object value, ColumnMetadata column){
        return encode(value);
    }

    default D decode(Object value, ColumnMetadata column){
        return decode(value);
    }
}
