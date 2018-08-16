.. _stdlib-graphics:

.. module:: Base.Graphics

**********
 Graphics
**********

The ``Base.Graphics`` interface is an abstract wrapper; specific packages (e.g., Cairo and Tk/Gtk) implement much of the functionality.

Geometry
--------

.. function:: Vec2(x,y)

   Creates a point in two dimensions

.. function:: BoundingBox(xmin, xmax, ymin, ymax)

   Creates a box in two dimensions with the given edges

.. function:: BoundingBox(objs...)

   Creates a box in two dimensions that encloses all objects

.. function:: width(obj)

   Computes the width of an object

.. function:: height(obj)

   Computes the height of an object

.. function:: xmin(obj)

   Computes the minimum x-coordinate contained in an object

.. function:: xmax(obj)

   Computes the maximum x-coordinate contained in an object

.. function:: ymin(obj)

   Computes the minimum y-coordinate contained in an object

.. function:: ymax(obj)

   Computes the maximum y-coordinate contained in an object

.. function:: diagonal(obj)

   Return the length of the diagonal of an object

.. function:: aspect_ratio(obj)

   Compute the height/width of an object

.. function:: center(obj)

   Return the point in the center of an object

.. function:: xrange(obj)

   Returns a tuple ``(xmin(obj), xmax(obj))``

.. function:: yrange(obj)

   Returns a tuple ``(ymin(obj), ymax(obj))``

.. function:: rotate(obj, angle, origin) -> newobj

   Rotates an object around origin by the specified angle (radians),
   returning a new object of the same type.  Because of
   type-constancy, this new object may not always be a strict
   geometric rotation of the input; for example, if ``obj`` is a
   ``BoundingBox`` the return is the smallest ``BoundingBox`` that encloses
   the rotated input.

.. function:: shift(obj, dx, dy)

   Returns an object shifted horizontally and vertically by the indicated amounts

.. function:: *(obj, s::Real)
   :noindex:

   Scale the width and height of a graphics object, keeping the center fixed

.. function:: +(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox
   :noindex:

   Returns the smallest box containing both boxes

.. function:: &(bb1::BoundingBox, bb2::BoundingBox) -> BoundingBox
   :noindex:

   Returns the intersection, the largest box contained in both boxes

.. function:: deform(bb::BoundingBox, dxmin, dxmax, dymin, dymax)

   Returns a bounding box with all edges shifted by the indicated amounts

.. function:: isinside(bb::BoundingBox, x, y)

   True if the given point is inside the box

.. function:: isinside(bb::BoundingBox, point)

   True if the given point is inside the box
