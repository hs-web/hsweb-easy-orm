package org.hswebframework.ezorm.core;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class MethodReferenceInfo {
    private String column;
    private String method;
    private Class<?> owner;
}