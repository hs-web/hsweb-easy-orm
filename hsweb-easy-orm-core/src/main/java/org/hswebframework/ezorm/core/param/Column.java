package org.hswebframework.ezorm.core.param;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * @author zhouhao
 * @since 1.1
 */
@Getter
@Setter
@EqualsAndHashCode
public class Column implements Serializable {
    private String name;

    private String type;
}
