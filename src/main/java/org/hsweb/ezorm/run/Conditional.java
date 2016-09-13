/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.run;

import org.hsweb.ezorm.param.Term;

public interface Conditional<T extends Conditional> {
    T where(String condition, Object value);

    T and(String condition, Object value);

    T or(String condition, Object value);

    Term nest();

    Term nest(String condition, Object value);

    Term orNest();

    Term orNest(String condition, Object value);
}
