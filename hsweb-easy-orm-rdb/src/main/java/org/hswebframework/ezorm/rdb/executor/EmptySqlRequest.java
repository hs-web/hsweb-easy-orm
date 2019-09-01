package org.hswebframework.ezorm.rdb.executor;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class EmptySqlRequest implements SqlRequest {

    public static final EmptySqlRequest INSTANCE=new EmptySqlRequest();

    @Override
    public String getSql() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Object[] getParameters() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        return "empty sql";
    }
}
