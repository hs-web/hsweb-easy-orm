package org.hswebframework.ezorm.core;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Setter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@Setter
public class DefaultExtensible implements Extensible {

    private Map<String, Object> extensions;

    @JsonIgnore
    public Map<String, Object> getExtensions() {
        return extensions;
    }

    @Override
    @JsonAnyGetter
    public Map<String, Object> extensions() {
        return extensions == null ? Collections.emptyMap() : extensions;
    }

    @Override
    @JsonAnySetter
    public synchronized void setExtension(String property, Object value) {
        if (extensions == null) {
            extensions = new HashMap<>();
        }
        extensions.put(property, value);
    }
}
