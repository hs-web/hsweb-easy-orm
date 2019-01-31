package org.hswebframework.ezorm.core;

import java.util.List;

/**
 * @author zhouhao
 * @since 1.0.0
 */
public interface TermSupplier {
    String getColumn();

    String getTermType();

    List<String> getOptions();

    Object getValue();
}
